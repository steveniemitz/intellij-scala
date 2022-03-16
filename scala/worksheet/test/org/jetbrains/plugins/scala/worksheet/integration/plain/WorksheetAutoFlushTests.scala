package org.jetbrains.plugins.scala.worksheet.integration.plain

import org.jetbrains.plugins.scala.FlakyTests
import org.jetbrains.plugins.scala.extensions.StringExt
import org.jetbrains.plugins.scala.util.RevertableChange
import org.jetbrains.plugins.scala.worksheet.actions.topmenu.RunWorksheetAction.RunWorksheetActionResult
import org.jetbrains.plugins.scala.worksheet.integration.WorksheetIntegrationBaseTest.{Folding, ViewerEditorData}
import org.jetbrains.plugins.scala.worksheet.integration.{WorksheetIntegrationBaseTest, WorksheetRunTestSettings}
import org.jetbrains.plugins.scala.worksheet.runconfiguration.WorksheetCache
import org.jetbrains.plugins.scala.worksheet.settings.WorksheetExternalRunType
import org.jetbrains.plugins.scala.worksheet.ui.printers.WorksheetEditorPrinterPlain.{FoldingDataForTests, ViewerEditorState}
import org.jetbrains.plugins.scala.worksheet.ui.printers.{WorksheetEditorPrinterFactory, WorksheetEditorPrinterPlain}
import org.junit.Assert.{assertEquals, assertTrue, fail}
import org.junit.experimental.categories.Category

import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}

@Category(Array(classOf[FlakyTests]))
class WorksheetPlainCompileOnServerRunLocallyAutoFlushTest extends WorksheetPlainAutoFlushTestBase {
  override def useCompileServer = true
  override def runInCompileServerProcess = false
}

@Category(Array(classOf[FlakyTests]))
class WorksheetPlainCompileLocallyRunLocallyAutoFlushTest extends WorksheetPlainAutoFlushTestBase {
  override def useCompileServer = false
  override def runInCompileServerProcess = false
}

abstract class WorksheetPlainAutoFlushTestBase extends WorksheetIntegrationBaseTest with WorksheetRunTestSettings {
  override def runType: WorksheetExternalRunType = WorksheetExternalRunType.PlainRunType

  def testAutoFlushOnLongEvaluation_DefaultAutoFlushTimeout(): Unit =
    doTestAutoFlushOnLongEvaluationNTimes(
      timesToRunTest = 3,
      autoFlushTimeout = WorksheetEditorPrinterFactory.IDLE_TIME,
      sleepInLoop = WorksheetEditorPrinterFactory.IDLE_TIME.mul(1.1)
    )

  // flush timeout is currently not intended to be changed by user via any setting,
  // but this test helps catching flaky tests caused by concurrency bugs
  def testAutoFlushOnLongEvaluation_SmallAutoFlushTimeout(): Unit =
    doTestAutoFlushOnLongEvaluationNTimes(
      timesToRunTest = 5,
      autoFlushTimeout = 10.millis,
      sleepInLoop = 100.millis
    )

  /** @param timesToRunTest number of times to rerun the test in case it succeeds. Used to catch flaky tests */
  private def doTestAutoFlushOnLongEvaluationNTimes(
    timesToRunTest: Int,
    autoFlushTimeout: FiniteDuration,
    sleepInLoop: Duration,
  ): Unit = {
    val revertible = RevertableChange.withModifiedSetting2(
      WorksheetEditorPrinterFactory
    )(autoFlushTimeout)(_.IDLE_TIME, _.IDLE_TIME = _)

    revertible.run {
      val attempts = timesToRunTest
      for (attempt <- 1 to attempts) {
        println(s"Test run attempt $attempt (out of $attempts)")
        try doTestAutoFlushOnLongEvaluation(sleepInLoop) catch {
          case e: Throwable =>
            throw e
        }
      }
    }
  }
  private def doTestAutoFlushOnLongEvaluation(sleepInLoop: Duration): Unit = {
    val sleepTime: Int = sleepInLoop.toMillis.toInt
    val leftText =
      s"""println("a\\nb\\nc")
         |
         |def foo(attempt: Int) = {
         |  for (i <- 1 to 3) {
         |    println(s"Hello $${attempt}-$$i")
         |    Thread.sleep($sleepTime)
         |  }
         |}
         |
         |foo(1)
         |foo(2)
         |""".stripMargin // TODO: extra foo()

    val lastStateExpected =
      s"""${foldStart}a
         |b
         |c
         |res0: Unit = ()$foldEnd
         |
         |foo: foo[](val attempt: Int) => Unit
         |
         |
         |
         |
         |
         |
         |${foldStart}Hello 1-1
         |Hello 1-2
         |Hello 1-3
         |res1: Unit = ()$foldEnd
         |${foldStart}Hello 2-1
         |Hello 2-2
         |Hello 2-3
         |res2: Unit = ()$foldEnd""".stripMargin.withNormalizedSeparator

    val viewerStates: Seq[ViewerEditorData] = runLongEvaluation(leftText).distinct

    def statesText(statesRendered: Seq[String]): String = {
      statesRendered.zipWithIndex.map { case (state, idx) => s"##### $idx:\n$state" }.mkString("\n")
    }

    val flushAtLeast = 1
    assertTrue(
      s"""editor should be flushed at least one time, states:
         |${statesText(viewerStates.map(renderViewerData))}""".stripMargin,
      viewerStates.nonEmpty
    )

    assertEquals(
      "final editor state doesn't match",
      lastStateExpected,
      renderViewerData(viewerStates.last)
    )
  }


  private def renderViewerData(viewerData: ViewerEditorData): String = {
    val text = viewerData.text
    val foldings = viewerData.foldings
    val builder = new java.lang.StringBuilder()

    val foldingsWithHelpers = Folding(0, 0) +: foldings :+ Folding(text.length, text.length)
    foldingsWithHelpers.sliding(2).foreach { case Seq(prev, Folding(startOffset, endOffset, isExpanded)) =>
      builder.append(text, prev.endOffset, startOffset)
      val isHelperFolding = startOffset == endOffset && startOffset == text.length
      if (!isHelperFolding) {
        builder.append(if (isExpanded) foldStartExpanded else foldStart)
          //.append(placeholder)
          .append(text, startOffset, endOffset)
          .append(if (isExpanded) foldEndExpanded else foldEnd)
      }
    }
    builder.toString
  }

  private def runLongEvaluation(leftText: String): Seq[ViewerEditorData] = {
    val editor = prepareWorksheetEditor(leftText)

    val evaluationResult = waitForEvaluationEnd(runWorksheetEvaluation(editor))
    assertEquals(RunWorksheetActionResult.Done, evaluationResult)

    val printer = WorksheetCache.getInstance(project).getPrinter(editor)
      .getOrElse(fail("no printer found").asInstanceOf[Nothing]).asInstanceOf[WorksheetEditorPrinterPlain]
    val viewer = WorksheetCache.getInstance(project).getViewer(editor)

    val viewerStates: Seq[ViewerEditorData] =
      printer.viewerEditorStates.map { case ViewerEditorState(text, foldings) =>
        val foldingsConverted = foldings.map { case FoldingDataForTests(start, end, _, expanded) =>
          Folding(start, end, expanded)
        }
        ViewerEditorData(viewer, text, foldingsConverted)
      }.toSeq

    viewerStates
  }
}
