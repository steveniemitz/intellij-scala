package org.jetbrains.plugins.scala.worksheet.ui.printers

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.impl.EditorImpl
import com.intellij.openapi.editor.{Document, Editor}
import com.intellij.openapi.util.text.StringUtil
import com.intellij.util.concurrency.annotations.{RequiresEdt, RequiresWriteLock}
import org.apache.commons.lang3.StringUtils
import org.jetbrains.annotations.TestOnly
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.worksheet.processor.WorksheetDefaultSourcePreprocessor
import org.jetbrains.plugins.scala.worksheet.processor.WorksheetDefaultSourcePreprocessor.ServiceMarkers
import org.jetbrains.plugins.scala.worksheet.ui.printers.WorksheetEditorPrinterBase.InputOutputFoldingInfo
import org.jetbrains.plugins.scala.worksheet.ui.printers.WorksheetEditorPrinterPlain._

import java.util.concurrent.locks.ReentrantLock
import javax.swing.Timer
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Promise
import scala.util.Success

final class WorksheetEditorPrinterPlain private[printers](
  editor: Editor,
  viewer: Editor,
  file: ScalaFile
) extends WorksheetEditorPrinterBase(editor, viewer) {
  private val myLock = new ReentrantLock(false)

  // used to flush collected output if there is some long process generating running
  private val flushTimer = new Timer(WorksheetEditorPrinterFactory.IDLE_TIME.toMillis.toInt, _ => flushOnTimer())

  private val evaluatedChunks = ArrayBuffer[EvaluationChunk]()

  private val currentOutputBuffer = new mutable.StringBuilder()
  private var currentOutputNewLinesCount = 0
  private var currentResultStartLine: Option[String] = None

  private var cutoffPrinted = false
  @volatile private var firstLineAccepted = false
  @volatile private var buffed = 0

  @volatile private var terminated = false
  override val processingTerminatedPromise: Promise[Unit] = Promise.apply()
  private def terminate(): Unit = {
    //debug("terminate")
    flushBuffer()
    terminated = true
    processingTerminatedPromise.complete(Success(()))
  }

  @TestOnly
  lazy val viewerEditorStates: ArrayBuffer[ViewerEditorState] = ArrayBuffer.empty

  originalEditor.asInstanceOf[EditorImpl].setScrollToCaret(false)
  worksheetViewer.asInstanceOf[EditorImpl].setScrollToCaret(false)

  override def getScalaFile: ScalaFile = file

  /** @param line single worksheet output line, currently expecting with '\n' in the end */
  override def processLine(line: String): Boolean = try myLock.locked {
    if (!firstLineAccepted) {
      firstLineAccepted = true
      //debug("first line accepted, starting flush timer")
      flushTimer.start()
    }

    //debug(s"processLine start: ${line.replaceAll("\n", " \\\\n ")}")
    //Thread.sleep(20) // for concurrency issue debugging
    if (isTerminationLine(line)) {
      terminate()
      return true
    }

    if (isResultStart(line)) {
      currentResultStartLine = Some(line)
    } else if (isResultEnd(line)) {
      if (!isInited) {
        init()
      }

      WorksheetDefaultSourcePreprocessor.inputLinesRangeFromEnd(line) match {
        case Some((inputStartLine, inputEndLine)) =>
          val output = currentOutputBufferText
          val chunk  = EvaluationChunk(inputStartLine, inputEndLine, output)
          evaluatedChunks += chunk
        case _ =>
      }

      currentOutputBuffer.clear()
      currentOutputNewLinesCount = 0
      cutoffPrinted = false
    } else {
      // TODO BULK_COUNT is unused
      if (currentOutputNewLinesCount < WorksheetEditorPrinterFactory.BULK_COUNT) {
        currentOutputBuffer.append(line)
      } else if (!cutoffPrinted) {
        currentOutputBuffer.append(WorksheetEditorPrinterFactory.END_MESSAGE)
        cutoffPrinted = true
      }

      currentOutputNewLinesCount += 1
      buffed += 1
    }

    false
  } finally {
    //debug(s"processLine end")
  }

  override def internalError(ex: Throwable): Unit = myLock.locked {
    terminate()
    super.internalError(ex)
    stopTimer()
  }

  override def flushBuffer(): Unit = myLock.locked {
    if (!isInited) {
      init()
    }
    if (terminated) {
      return
    }

    stopTimer()

    flushContentSync()

    invokeAndWait {
      worksheetViewer.getMarkupModel.removeAllHighlighters()

      inWriteAction {
        saveEvaluationResult(viewerDocument.getText)
      }
    }
  }

  @volatile
  private var flushOnTimerInProcess = false

  // currently we re-render text on each mid-flush (~once per 1 second for long processes),
  // for now we are ok with this cause `renderText` proved to be quite a lightweight operation
  // Called from timer, so body invoked in EDT
  @RequiresEdt // it's not required, but expected to be called from EDT (which is used for Timer callback execution)
  private def flushOnTimer(): Unit = {
    //debug("flushOnTimer start")
    if (!firstLineAccepted) {
      //debug("flushOnTimer skip (no process output yet)")
    }
    else if (terminated) {
      //debug("flushOnTimer skip (printer is already terminated)")
    }
    else if (flushOnTimerInProcess) {
      //debug("flushOnTimer skip (flushing on timer is already in process)")
    }
    else {
      flushOnTimerInProcess = true
      executeOnPooledThread {
        //debug("flushOnTimer continue in pooled thread")
        try {
          flushContentSync()
        } finally {
          //debug("flushOnTimer end (flushed)")
          flushOnTimerInProcess = false
        }
      }
    }
  }

  private def flushContentSync(): Unit = myLock.locked {
    //debug("flushContentSync start")
    Log.assertTrue(
      !ApplicationManager.getApplication.isDispatchThread,
      "flushContent should not be called on EDT to avoid deadlocks"
    )

    if (buffed == 0) {
      //debug("flushContentSync end (nothing to flush)")
    }
    else try {
      val lastChunkOpt = buildIncompleteLastChunkOpt
      val (text, foldings) = renderText(evaluatedChunks ++ lastChunkOpt)


      invokeAndWait {
        val expandedFoldingsIds = foldGroup.indexesOfExpandedRegions
        foldings.iterator.zipWithIndex.foreach { case (folding, idx) =>
          if (expandedFoldingsIds.contains(idx))
            folding.isExpanded = true
        }

        inWriteAction {
          updateViewerDocumentTextWithPersistentScroll(viewerDocument, text, foldings)
        }
      }
    } finally {
      //debug("flushContentSync end")
    }
  }

  override def close(): Unit =
    stopTimer()

  private def stopTimer(): Unit =
    if (flushTimer.isRunning) {
      flushTimer.stop()
    }

  private def buildIncompleteLastChunkOpt: Option[EvaluationChunk] = {
    if (StringUtils.isNotBlank(currentOutputBuffer)) {
      val (inputStartLine, inputEndLine) =
        currentResultStartLine.flatMap(WorksheetDefaultSourcePreprocessor.inputLinesRangeFromStart).getOrElse(0, 0)
      val output = currentOutputBufferText
      Some(EvaluationChunk(inputStartLine, inputEndLine, output))
    } else {
      None
    }
  }

  private def currentOutputBufferText: String =
    currentOutputBuffer.result().replaceFirst("\\s++$", "")

  // TODO: there can be a lot of worksheet output, make these checks mor efficient to lower GC usage
  private def isTerminationLine(line: String): Boolean =
    line.stripSuffix("\n") == ServiceMarkers.EVALUATION_END_MARKER

  private def isResultStart(line: String): Boolean =
    line.startsWith(ServiceMarkers.CHUNK_OUTPUT_START_MARKER)

  private def isResultEnd(line: String): Boolean =
    line.startsWith(ServiceMarkers.CHUNK_OUTPUT_END_MARKER)

  @RequiresWriteLock
  private def updateViewerDocumentTextWithPersistentScroll(
    viewerDocument: Document,
    text: CharSequence,
    foldings: Iterable[InputOutputFoldingInfo]
  ): Unit = {
    //def debugStatus(startOrEnd: String): Unit =
    //  debug(s"updateViewerDocumentTextWithPersistentScroll $startOrEnd, text length ${text.length()}, foldings count: ${foldings.size}")
    //debugStatus("start")

    val editorScroll = originalEditor.getScrollingModel.getVerticalScrollOffset
    val viewerScroll = worksheetViewer.getScrollingModel.getVerticalScrollOffset

    setDocumentTextAndCommit(viewerDocument, text)

    originalEditor.getScrollingModel.scrollVertically(editorScroll)
    worksheetViewer.getScrollingModel.scrollHorizontally(viewerScroll)

    // NOTE: if a folding already exists in a folding group it will note be duplicated
    // see FoldingModelImpl.createFoldRegion
    cleanFoldings()
    updateFoldings(foldings)
    foldGroup.initMappings()

    if (ApplicationManager.getApplication.isUnitTestMode) {
      val actualFoldings = viewer.getFoldingModel.getAllFoldRegions.map { f =>
        FoldingDataForTests(f.getStartOffset, f.getEndOffset, f.getPlaceholderText, f.isExpanded)
      }
      viewerEditorStates += ViewerEditorState(viewerDocument.getText, actualFoldings.toSeq)
    }

    //debugStatus("end")
  }
}

object WorksheetEditorPrinterPlain {

  private val Log = Logger.getInstance(classOf[WorksheetEditorPrinterPlain])

  /**
   * Represents evaluated expression which is located on `inputStartLine..inputEndLine` lines in the left editor
   * and which evaluation output equals to `outputText`
   */
  private case class EvaluationChunk(inputStartLine: Int,
                                     inputEndLine: Int,
                                     outputText: String) {

    def outputLinesCount: Int = StringUtil.countNewLines(outputText) + 1
  }

  /**
   * @return grouped of sequential chunks, each group represent chunks that go on a single line
   *         meaning that left sibling chunk end line equals to right sibling chunk start line
   *         (this can happen when expressions are separated with comma)
   *
   * @example in format (lineStart, lineEnd): <br>
   *          input: Seq((1, 1), (1, 1), (1, 2), (2, 2), (3, 4), (4, 5)) <br>
   *          output: Seq(Seq((1, 1), (1, 1), (1, 2), (2, 2)), Seq((3, 4), (4, 5))) <br>
   */
  private def groupChunks(chunks: collection.Seq[EvaluationChunk]): Iterable[Iterable[EvaluationChunk]] =
    if (chunks.isEmpty) Seq()
    else {
      val result = ArrayBuffer(ArrayBuffer(chunks.head))

      chunks.sliding(2).foreach {
        case collection.Seq(prev, curr) =>
          def logExtraDebugInfo = {
            val application = ApplicationManager.getApplication
            application.isUnitTestMode || application.isInternal
          }
          def extraDebugInfo =
            if (logExtraDebugInfo) chunks.mkString("chunks:\n", "\n", "") else ""

          Log.assertTrue(
            prev.inputEndLine <= curr.inputStartLine,
            s"""chunks should be ordered:
               |prev: (${prev.inputStartLine}, ${prev.inputEndLine})
               |curr: (${curr.inputStartLine}, ${curr.inputEndLine})
               |$extraDebugInfo""".stripMargin
          )

          if (prev.inputEndLine == curr.inputStartLine) {
            result.last += curr
          } else {
            result += ArrayBuffer(curr)
          }
        case _ => // only one chunk is present for now
      }

      result
    }


  // TODO: unify with REPL printer, reuse concepts
  private def renderText(chunks: collection.Seq[EvaluationChunk]): (CharSequence, Seq[InputOutputFoldingInfo]) = {
    val resultText = new StringBuilder()
    val resultFoldingsBuilder = ArraySeq.newBuilder[InputOutputFoldingInfo]
    var foldedLines = 0

    val chunksGrouped: Iterable[Iterable[EvaluationChunk]] = WorksheetEditorPrinterPlain.groupChunks(chunks)

    for { group <- chunksGrouped.iterator } {
      val inputStartLine   = group.head.inputStartLine
      val inputEndLine     = group.last.inputEndLine
      val inputLinesCount  = inputEndLine - inputStartLine + 1
      val outputLinesCount = group.map(_.outputLinesCount).sum

      val totalOutputLinesCount        = StringUtil.countNewLines(resultText)
      val totalOutputVisibleLinesCount = totalOutputLinesCount - foldedLines

      // align visible output line in the right editor with current input line from the left editor
      val leadingNewLinesCount = {
        val diff = inputStartLine - totalOutputVisibleLinesCount
        if (diff < 0){
          // expecting visible lines to be folded with the last input end line, thus less then current input start line
          // NOTE: be careful not to log chunk text itself
          val chunksDump = chunks.map { case c@EvaluationChunk(s, e, t) => (s, e, t.length, c.outputLinesCount) }
          val message = s"leadingNewLinesCount is expected to be non-negative but got: $diff, chunks: $chunksDump"
          Log.warn(message)
        }
        diff.max(0)
      }
      if (leadingNewLinesCount > 0) {
        resultText.append("\n" * leadingNewLinesCount)
      }

      group.foreach { chunk =>
        resultText.append(chunk.outputText)
      }

      val diffLocal = outputLinesCount - inputLinesCount
      val needFolding = diffLocal > 0
      if (needFolding) {
        // current output is longer than input, need to fold some output lines to align with input start/end lines
        val outputStartLine = totalOutputLinesCount + leadingNewLinesCount
        val outputEndLine = outputStartLine + outputLinesCount - 1
        val folding = InputOutputFoldingInfo(
          inputStartLine,
          inputEndLine,
          outputStartLine,
          outputEndLine,
        )

        foldedLines += diffLocal
        resultFoldingsBuilder += folding
      } else if (diffLocal < 0) {
        // current input is longer than output need to add extra trailing spaces after the output
        // to align input end with output last line
        val trailingNewLinesCount = -diffLocal //+ 1
        resultText.append("\n" * trailingNewLinesCount)
      } else {
        // do nothing, input and output lines are already aligned
      }
    }

    (resultText, resultFoldingsBuilder.result())
  }

  @TestOnly
  case class ViewerEditorState(documentText: String, foldings: Seq[FoldingDataForTests])
  case class FoldingDataForTests(
    startOffset: Int,
    endOffset: Int,
    placeholderText: String,
    isFolded: Boolean
  )
}