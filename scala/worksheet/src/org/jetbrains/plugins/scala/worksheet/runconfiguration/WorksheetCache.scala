package org.jetbrains.plugins.scala.worksheet.runconfiguration

import com.intellij.openapi.Disposable
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.{Editor, EditorFactory}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.io.FileUtil
import com.intellij.util.containers.ContainerUtil
import org.jetbrains.annotations.TestOnly
import org.jetbrains.plugins.scala.worksheet.processor.WorksheetCompiler.CompilerMessagesCollector
import org.jetbrains.plugins.scala.worksheet.runconfiguration.WorksheetCache.MagicFlag
import org.jetbrains.plugins.scala.worksheet.ui.printers.{WorksheetEditorPrinter, WorksheetEditorPrinterRepl}

import java.io.File
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Try}

final class WorksheetCache extends Disposable {

  private val editorToViewer = ContainerUtil.createWeakMap[Editor, Editor]
  private val editorToPrinter = ContainerUtil.createWeakMap[Editor, WorksheetEditorPrinter]
  private val editorToMagicFlag  = ContainerUtil.createWeakMap[Editor, MagicFlag]

  private val Log: Logger = Logger.getInstance(getClass)

  @TestOnly
  private val allCompilerMessagesCollectors = ContainerUtil.createWeakMap[Editor, CompilerMessagesCollector]()

  // TODO: cleanup created files on application/project exit, do not pollute file system!
  private val compilationInfo = mutable.HashMap.empty[String, (Int, File, File)]

  def updateOrCreateCompilationInfo(filePath: String, fileName: String): (Int, File, File) =
    updateOrCreateCompilationInfo(filePath, fileName, None)

  def updateOrCreateCompilationInfo(filePath: String, fileName: String, tempDirName: Option[String]): (Int, File, File) =
    compilationInfo.get(filePath) match {
      case Some(result@(it, src, out)) if src.exists() && out.exists() =>
        compilationInfo.put(filePath, (it + 1, src, out))
        result
      case _ =>
        val tempDirAbsolute = tempDirName match {
          case Some(tempDir) => new File(FileUtil.getTempDirectory, tempDir)
          case _             => new File(FileUtil.getTempDirectory)
        }
        val src = FileUtil.createTempFile(tempDirAbsolute, fileName, null, true)
        val out = FileUtil.createTempDirectory(tempDirAbsolute, fileName, null, true)
        compilationInfo.put(filePath, (1, src, out))
        (0, src, out)
    }

  @TestOnly
  def getCompilerMessagesCollector(inputEditor: Editor): Option[CompilerMessagesCollector] =
    Option(allCompilerMessagesCollectors.get(inputEditor))

  @TestOnly
  def addCompilerMessagesCollector(inputEditor: Editor, collector: CompilerMessagesCollector): Unit =
    allCompilerMessagesCollectors.put(inputEditor, collector)

  def peakCompilationIteration(filePath: String): Int =
    compilationInfo.get(filePath).map(_._1).getOrElse(-1)

  def getPrinter(inputEditor: Editor): Option[WorksheetEditorPrinter] =
    Option(editorToPrinter.get(inputEditor))

  def setPrinter(inputEditor: Editor, printer: WorksheetEditorPrinter): Unit =
    editorToPrinter.put(inputEditor, printer)

  def removePrinter(inputEditor: Editor): Unit = {
    val removed = editorToPrinter.remove(inputEditor)
    if (removed != null) {
      removed.close()
    }
  }

  def getLastProcessedIncremental(inputEditor: Editor): Option[Int] =
    Option(editorToPrinter.get(inputEditor)).flatMap {
      case in: WorksheetEditorPrinterRepl => in.lastProcessedLine
      case _                              => None
    }

  def resetLastProcessedIncremental(inputEditor: Editor): Unit =
    editorToPrinter.get(inputEditor) match {
      case inc: WorksheetEditorPrinterRepl => inc.resetLastProcessedLine()
      case _                               =>
    }

  def getPatchedFlag(editor: Editor): Option[MagicFlag] =
    Option(editorToMagicFlag.get(editor))

  def setPatchedFlag(editor: Editor, flag: MagicFlag): Unit =
    editorToMagicFlag.put(editor, flag)

  def removePatchedFlag(editor: Editor): Unit =
    editorToMagicFlag.remove(editor)

  def getViewer(editor: Editor): Editor = synchronized {
    val viewer = editorToViewer.get(editor)

    if (viewer != null && viewer.isDisposed || editor.isDisposed) {
      editorToViewer.remove(editor)
      null
    }
    else viewer
  }

  def setViewer(editor: Editor, viewer: Editor): Unit =
    synchronized {
      editorToViewer.put(editor, viewer)
    }

  override def dispose(): Unit = {
    invalidatePrinters()
    invalidateViewers()
  }

  private def logExceptions[T](body: => T): Unit =
    Try(body) match {
      case Failure(exception) =>
        Log.error(exception)
      case _=>
    }

  private def invalidatePrinters(): Unit = {
    for {
      printer <- editorToPrinter.asScala.values
    } logExceptions(printer.close())
    editorToPrinter.clear()
  }

  private def invalidateViewers(): Unit = {
    val factory = EditorFactory.getInstance()
    for {
      viewer <- editorToViewer.values.asScala
      if !viewer.isDisposed
    } logExceptions(factory.releaseEditor(viewer))
    editorToViewer.clear()
  }
}

object WorksheetCache {
  def getInstance(project: Project): WorksheetCache = project.getService(classOf[WorksheetCache])

  //the purpose of this is unclear, previously a String was used with 2 possible magic values
  sealed trait MagicFlag
  object MagicFlag {
    object MagicValue1 extends MagicFlag
    object MagicValue2 extends MagicFlag
  }
}