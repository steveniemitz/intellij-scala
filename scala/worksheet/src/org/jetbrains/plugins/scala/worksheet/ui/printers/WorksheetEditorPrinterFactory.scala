package org.jetbrains.plugins.scala.worksheet.ui.printers

import com.intellij.diff.tools.util.BaseSyncScrollable
import com.intellij.diff.tools.util.SyncScrollSupport.TwosideSyncScrollSupport
import com.intellij.ide.DataManager
import com.intellij.openapi.actionSystem.{CommonDataKeys, DataProvider}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.event.{CaretEvent, CaretListener, VisibleAreaEvent, VisibleAreaListener}
import com.intellij.openapi.editor.impl.EditorImpl
import com.intellij.openapi.editor.{Editor, EditorFactory, VisualPosition}
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.Splitter
import com.intellij.openapi.util.Key
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.openapi.vfs.newvfs.FileAttribute
import com.intellij.ui.JBSplitter
import com.intellij.util.concurrency.annotations.RequiresEdt
import org.jetbrains.annotations.TestOnly
import org.jetbrains.plugins.scala.extensions.{IteratorExt, StringExt, invokeLater}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.util.ui.extensions.JComponentExt
import org.jetbrains.plugins.scala.worksheet.WorksheetBundle
import org.jetbrains.plugins.scala.worksheet.runconfiguration.WorksheetCache
import org.jetbrains.plugins.scala.worksheet.runconfiguration.WorksheetCache.MagicFlag.{MagicValue1, MagicValue2}
import org.jetbrains.plugins.scala.worksheet.ui.WorksheetDiffSplitters.SimpleWorksheetSplitter
import org.jetbrains.plugins.scala.worksheet.ui.{WorksheetDiffSplitters, WorksheetFoldGroup}
import org.jetbrains.plugins.scala.worksheet.utils.FileAttributeUtilCache

import java.awt.{BorderLayout, Dimension}
import java.util
import javax.swing.{JComponent, JLayeredPane}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

//noinspection TypeAnnotation
object WorksheetEditorPrinterFactory {
  private val Log = Logger.getInstance(this.getClass)

  val END_MESSAGE = WorksheetBundle.message("worksheet.printers.output.exceeds.cutoff.limit") + "\n"
  val BULK_COUNT = 15 // TODO: add a setting

  private var _IDLE_TIME: FiniteDuration = 1000.millis
  def IDLE_TIME: FiniteDuration = _IDLE_TIME
  @TestOnly
  def IDLE_TIME_=(value: FiniteDuration): Unit = _IDLE_TIME = value


  val DEFAULT_WORKSHEET_VIEWERS_RATIO = 0.5f
  val DIFF_SPLITTER_KEY: Key[SimpleWorksheetSplitter] = Key.create[SimpleWorksheetSplitter]("SimpleWorksheetViewerSplitter")

  private val LAST_WORKSHEET_RUN_RESULT = new FileAttribute("LastWorksheetRunResult", 2, false)
  private val LAST_WORKSHEET_RUN_RATIO = new FileAttribute("ScalaWorksheetLastRatio", 1, false)

  def synch(
    editor: Editor,
    viewer: Editor,
    diffSplitter: Option[SimpleWorksheetSplitter] = None,
    foldGroup: Option[WorksheetFoldGroup] = None
  ): Unit = {
    (editor, viewer) match {
      case (editorImpl: EditorImpl, viewerImpl: EditorImpl) =>
        invokeLater {
          synchImpl(editorImpl, viewerImpl, diffSplitter, foldGroup)
        }
      case _ =>
        Log.warn(s"editor or viewer doesn't extend EditorImpl ($editor)")
    }
  }

  @RequiresEdt
  private def synchImpl(
    editor: EditorImpl,
    viewer: EditorImpl,
    diffSplitter: Option[SimpleWorksheetSplitter] = None,
    foldGroup: Option[WorksheetFoldGroup] = None
  ): Unit = {
    val cache = WorksheetCache.getInstance(editor.getProject)

    cache.getPatchedFlag(editor) match {
      case Some(MagicValue1) | None =>
        cache.removePatchedFlag(editor)
        val caretListener = createCaretListener(editor, viewer, foldGroup)
        editor.getCaretModel.addCaretListener(caretListener)
        val newMagicValue = if (foldGroup.isDefined) MagicValue2 else MagicValue1
        cache.setPatchedFlag(editor, newMagicValue)
      case _ =>
    }

    val editorVisualLine = editor.getCaretModel.getVisualPosition.getLine
    val viewerLastLine = viewer.getDocument.getLineCount
    val viewerNewVisualLine = Math.min(editorVisualLine, viewerLastLine)
    viewer.getCaretModel.moveToVisualPosition(new VisualPosition(viewerNewVisualLine, 0))

    val syncSupport = new TwosideSyncScrollSupport(
      util.Arrays.asList(editor, viewer),
      NoopSyncScrollable
    )

    diffSplitter.foreach { splitter =>
      val listener: VisibleAreaListener = (e: VisibleAreaEvent) => {
        splitter.redrawDiffs()
        syncSupport.visibleAreaChanged(e)
      }

      editor.getScrollingModel.addVisibleAreaListener(listener)
      viewer.getScrollingModel.addVisibleAreaListener(listener)
    }
  }

  private def createCaretListener(editor: Editor, viewer: Editor, foldGroup: Option[WorksheetFoldGroup]): CaretListener = foldGroup match {
    case Some(group) =>
      new CaretListener {
        override def caretPositionChanged(e: CaretEvent): Unit = {
          if (e.getEditor.asInstanceOf[EditorImpl].getContentComponent.hasFocus) {
            val editorVisualLine = editor.getCaretModel.getVisualPosition.getLine
            val viewerLastLine = viewer.getDocument.getLineCount
            val line = Math.min(group.left2rightOffset(editorVisualLine), viewerLastLine)
            viewer.getCaretModel.moveToVisualPosition(new VisualPosition(line, 0))
          }
        }
      }

    case _ =>
      new CaretListener {
        override def caretPositionChanged(e: CaretEvent): Unit = {
          if (e.getEditor.asInstanceOf[EditorImpl].getContentComponent.hasFocus) {
            viewer.getCaretModel.moveToVisualPosition(editor.getCaretModel.getVisualPosition)
          }
        }
      }
  }

  private object NoopSyncScrollable extends BaseSyncScrollable {
    override def processHelper(scrollHelper: BaseSyncScrollable.ScrollHelper): Unit = ()
    override def isSyncScrollEnabled: Boolean = true
  }

  def saveWorksheetEvaluation(file: VirtualFile, result: String, ratio: Float): Unit = {
    FileAttributeUtilCache.writeAttribute(LAST_WORKSHEET_RUN_RESULT, file, result)
    FileAttributeUtilCache.writeAttribute(LAST_WORKSHEET_RUN_RATIO, file, ratio.toString)
  }

  def saveOnlyRatio(file: VirtualFile, ratio: Float): Unit =
    FileAttributeUtilCache.writeAttribute(LAST_WORKSHEET_RUN_RATIO, file, ratio.toString)

  def loadWorksheetEvaluation(file: VirtualFile): Option[(String, Float)] = {
    val ratioAttribute = FileAttributeUtilCache.readAttribute(LAST_WORKSHEET_RUN_RATIO, file)
    val ratio = ratioAttribute.flatMap(_.toFloatOpt).getOrElse(DEFAULT_WORKSHEET_VIEWERS_RATIO)
    FileAttributeUtilCache.readAttribute(LAST_WORKSHEET_RUN_RESULT, file).map(s => (s, ratio))
  }

  def deleteWorksheetEvaluation(file: VirtualFile): Unit = {
    FileAttributeUtilCache.writeAttribute(LAST_WORKSHEET_RUN_RESULT, file, "")
    FileAttributeUtilCache.writeAttribute(LAST_WORKSHEET_RUN_RATIO, file, DEFAULT_WORKSHEET_VIEWERS_RATIO.toString)
  }

  def createViewer(editor: Editor): Editor = {
    val viewer = getOrCreateViewerEditorFor(editor)
    setupRightSideViewer(editor, viewer.asInstanceOf[EditorImpl], modelSync = true)
    viewer
  }

  def getDefaultUiFor(editor: Editor, scalaFile: ScalaFile): WorksheetEditorPrinter = {
    val printer = newDefaultUiFor(editor, scalaFile)
    val cache = WorksheetCache.getInstance(editor.getProject)
    cache.setPrinter(editor, printer)
    printer
  }

  def getIncrementalUiFor(editor: Editor, scalaFile: ScalaFile): WorksheetEditorPrinter = {
    val cache = WorksheetCache.getInstance(editor.getProject)

    cache.getPrinter(editor) match {
      case Some(printer: WorksheetEditorPrinterRepl) =>
        printer.updateScalaFile(scalaFile)
        printer
      case _  =>
        val printer = newIncrementalUiFor(editor, scalaFile)
        cache.setPrinter(editor, printer)
        printer
    }
  }

  private def newDefaultUiFor(editor: Editor, scalaFile: ScalaFile): WorksheetEditorPrinterPlain = {
    val viewer = getOrCreateViewerEditorFor(editor)
    setupRightSideViewer(editor, viewer.asInstanceOf[EditorImpl])
    new WorksheetEditorPrinterPlain(editor, viewer, scalaFile)
  }

  private def newIncrementalUiFor(editor: Editor, scalaFile: ScalaFile): WorksheetEditorPrinterRepl = {
    val viewer = getOrCreateViewerEditorFor(editor)
    setupRightSideViewer(editor, viewer.asInstanceOf[EditorImpl])
    new WorksheetEditorPrinterRepl(editor, viewer, scalaFile)
  }

  private def setupRightSideViewer(
    editor: Editor,
    viewer: EditorImpl,
    modelSync: Boolean = false
  ): Unit = {
    val editorComponent = editor.getComponent
    val editorContentComponent = editor.getContentComponent

    val viewerSettings = viewer.getSettings
    viewerSettings.setLineMarkerAreaShown(false)
    viewerSettings.setLineNumbersShown(false)

    val prop = editorComponent.components.headOption
      .collect { case splitter: JBSplitter => splitter.getProportion }
      .getOrElse(DEFAULT_WORKSHEET_VIEWERS_RATIO)

    val dimension = editorComponent.getSize()
    val prefDim = new Dimension(dimension.width / 2, dimension.height)

    editor.getSettings.setFoldingOutlineShown(false)

    viewer.getComponent.setPreferredSize(prefDim)

    if (modelSync) {
      synch(editor, viewer)
    }
    editorContentComponent.setPreferredSize(prefDim)

    val child = editorComponent.getParent

    val diffPane = WorksheetDiffSplitters.createSimpleSplitter(editor, viewer, prop)
    viewer.putUserData(DIFF_SPLITTER_KEY, diffPane)

    if (!ApplicationManager.getApplication.isUnitTestMode) {
      val parent = child.getParent

      @inline def preserveFocus(body: => Unit): Unit = {
        val hadFocus = editorContentComponent.hasFocus

        body

        if (hadFocus) editorContentComponent.requestFocusInWindow()
      }

      @inline def patchEditor(): Unit = preserveFocus {
        (parent, child) match {
          case (parentPane: JLayeredPane, _) =>
            parentPane.remove(child)
            parentPane.add(diffPane, BorderLayout.CENTER)
          case (_, childPane: JLayeredPane) =>
            childPane.remove(editorComponent)
            childPane.add(diffPane, BorderLayout.CENTER)
          case _ =>
        }
      }

      if (parent.getComponentCount > 1) {
        parent.getComponent(1) match {
          case _: Splitter =>
            preserveFocus {
              parent.remove(1)
              parent.add(diffPane, 1)
            }
          case _ => patchEditor()
        }
      } else patchEditor()
    }
  }

  private def getOrCreateViewerEditorFor(editor: Editor): Editor = {
    val project = editor.getProject
    val cache = WorksheetCache.getInstance(project)
    cache.synchronized {
      val viewer = cache.getViewer(editor)
      viewer match {
        case editor: EditorImpl => editor
        case _                  =>
          val newViewer = createBlankViewerEditor(project)
          cache.setViewer(editor, newViewer)
          newViewer
      }
    }
  }

  private def createBlankViewerEditor(project: Project): Editor = {
    val factory: EditorFactory = EditorFactory.getInstance
    val editor: Editor = factory.createViewer(factory.createDocument(""), project)
    editor.setBorder(null)
    editor.getContentComponent.getParent match {
      case jComp: JComponent =>
        val dataProvider: DataProvider = (dataId: String) => {
          if (CommonDataKeys.HOST_EDITOR.is(dataId)) editor
          else null
        }
        jComp.putClientProperty(DataManager.CLIENT_PROPERTY_DATA_PROVIDER, dataProvider)
      case _ =>
    }
    editor
  }
}
