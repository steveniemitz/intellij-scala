package org.jetbrains.plugins.scala
package testingSupport

import com.intellij.execution.actions.{ConfigurationContext, RunConfigurationProducer}
import com.intellij.execution.configurations.{ConfigurationType, RunnerSettings}
import com.intellij.execution.executors.DefaultRunExecutor
import com.intellij.execution.impl.DefaultJavaProgramRunner
import com.intellij.execution.process.{ProcessHandler, ProcessListener}
import com.intellij.execution.runners.{ExecutionEnvironmentBuilder, ProgramRunner}
import com.intellij.execution.testframework.AbstractTestProxy
import com.intellij.execution.testframework.sm.runner.SMTRunnerEventsListener
import com.intellij.execution.testframework.sm.runner.ui.SMTRunnerConsoleView
import com.intellij.execution.ui.RunContentDescriptor
import com.intellij.execution.{Executor, PsiLocation, RunnerAndConfigurationSettings}
import com.intellij.openapi.Disposable
import com.intellij.openapi.module.ModuleManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.impl.file.PsiDirectoryFactory
import com.intellij.psi.{PsiDirectory, PsiElement}
import com.intellij.testFramework.EdtTestUtil
import com.intellij.util.concurrency.Semaphore
import org.jetbrains.plugins.scala.base.ScalaSdkOwner
import org.jetbrains.plugins.scala.configurations.TestLocation.CaretLocation
import org.jetbrains.plugins.scala.debugger._
import org.jetbrains.plugins.scala.extensions.inReadAction
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.testingSupport.test.scalatest.ScalaTestRunConfiguration
import org.jetbrains.plugins.scala.testingSupport.test.specs2.Specs2RunConfiguration
import org.jetbrains.plugins.scala.testingSupport.test.utest.UTestRunConfiguration
import org.jetbrains.plugins.scala.testingSupport.test.{AbstractTestConfigurationProducer, AbstractTestRunConfiguration}
import org.jetbrains.plugins.scala.util.assertions.failWithCause
import org.junit.Assert._
import org.junit.experimental.categories.Category

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}

/**
 * @author Roman.Shein
 *         Date: 03.03.14
 */
@Category(Array(classOf[TestingSupportTests]))
abstract class ScalaTestingTestCase
  extends ScalaDebuggerTestBase
    with IntegrationTest
    with FileStructureTest
    with ScalaSdkOwner
    with TestOutputMarkers {

  override def useCompileServer: Boolean = false

  protected def configurationProducer: RunConfigurationProducer[_]

  override def runInDispatchThread(): Boolean = false

  /** if set to true, prints raw output of test process to console */
  final def debugProcessOutput = false

  override protected def addFileToProjectSources(fileName: String, fileText: String): VirtualFile =
    EdtTestUtil.runInEdtAndGet { () =>
      ScalaTestingTestCase.super.addFileToProjectSources(fileName, fileText)
    }

  override val testDataBasePrefix = "testingSupport"

  protected val useDynamicClassPath = false

  protected def createPsiLocation(location: CaretLocation): PsiLocation[PsiElement] =
    createPsiLocation(location, myModule, srcDir)

  private def failedConfigMessage(caretLocation: CaretLocation, reason: String): String = {
    val CaretLocation(fileName, line, column) = caretLocation
    s"""Failed to create run configuration for test from file $fileName from line $line at offset $column
       |Reason: $reason""".stripMargin
  }

  private def failedConfigMessage(packageName: String, reason: String = "<no reason>") =
    s"Failed to create run configuration for test from package $packageName\nReason: $reason"

  override protected def createTestFromCaretLocation(caretLocation: CaretLocation): RunnerAndConfigurationSettings =
    inReadAction {
      val psiElement = findPsiElement(caretLocation, getProject, srcDir)
      val context: ConfigurationContext = new ConfigurationContext(psiElement)
      val configurationFromContext = configurationProducer.createConfigurationFromContext(context)
      configurationFromContext.getConfigurationSettings
    }

  override protected def createTestFromPackage(packageName: String): RunnerAndConfigurationSettings =
    inReadAction {
      val psiPackage = ScalaPsiManager.instance(getProject).getCachedPackage(packageName)
      val psiDirectory = psiPackage.map(_.getDirectories().head) match {
        case Some(dir) => dir
        case None =>
          throw new RuntimeException(failedConfigMessage(packageName))
      }
      createTestFromDirectory(psiDirectory)
    }

  override protected def createTestFromModule(moduleName: String): RunnerAndConfigurationSettings =
    inReadAction {
      val manager = ModuleManager.getInstance(ScalaTestingTestCase.this.getProject)
      val module = manager.findModuleByName(moduleName)
      val moduleRoot = ModuleRootManager.getInstance(module).getContentRoots.head
      val directory = PsiDirectoryFactory.getInstance(getProject).createDirectory(moduleRoot)
      createTestFromDirectory(directory)
    }

  private def createTestFromDirectory(directory: PsiDirectory): RunnerAndConfigurationSettings = {
    inReadAction {

      val context: ConfigurationContext = new ConfigurationContext(directory)
      val configurationFromContext = configurationProducer.createConfigurationFromContext(context)
      configurationFromContext.getConfigurationSettings
    }
  }

  protected final def runTestFromConfig(
    runConfig: RunnerAndConfigurationSettings
  )(implicit testOptions: TestRunOptions): TestRunResult =
    runTestFromConfig(runConfig, testOptions.duration)

  override protected def runTestFromConfig(
    runConfig: RunnerAndConfigurationSettings,
    duration: FiniteDuration,
  ): TestRunResult = {
    val testResultListener = new TestRunnerOutputListener(debugProcessOutput)
    val testStatusListener = new TestStatusListener
    var testTreeRoot: Option[AbstractTestProxy] = None

    runConfig.getConfiguration.getProject
      .getMessageBus
      .connect(getTestRootDisposable)
      .subscribe(SMTRunnerEventsListener.TEST_STATUS, testStatusListener)

    val (handler, _) = EdtTestUtil.runInEdtAndGet(() => {
      if (needMake) {
        compiler.rebuild().assertNoProblems(allowWarnings = true)
        saveChecksums()
      }
      val runner = ProgramRunner.PROGRAM_RUNNER_EP.getExtensions.find(_.getClass == classOf[DefaultJavaProgramRunner]).get
      val (handler, runContentDescriptor) = runProcess(runConfig, classOf[DefaultRunExecutor], runner, Seq(testResultListener))

      runContentDescriptor.getExecutionConsole match {
        case descriptor: SMTRunnerConsoleView =>
          testTreeRoot = Some(descriptor.getResultsViewer.getRoot)
        case _ =>
      }
      (handler, runContentDescriptor)
    })

    val exitCode = waitForTestEnd(handler, duration)

    val result = TestRunResult(
      runConfig,
      exitCode.getOrElse(-1),
      ProcessOutput(
        testResultListener.outputText,
        testResultListener.outputTextFromTests,
        testStatusListener.uncapturedOutput,
      ),
      testTreeRoot,
    )

    exitCode match {
      case Failure(exception) =>
        result.printOutputDetailsToConsole()
        val message = s"test `${runConfig.getName}` did not terminate correctly after ${duration.toMillis} ms"
        failWithCause(message, exception)
      case _ =>
    }

    result
  }

  private def waitForTestEnd(
    handler: ProcessHandler,
    duration: FiniteDuration
  ): Try[Int] = {
    val exitCodeListener = new ProcessFinishedListener
    handler.addProcessListener(exitCodeListener)
    val exitCode = Try(Await.result(exitCodeListener.exitCodeFuture, duration))
    // in case of unprocessed output we want to wait for the process end until the project is disposed
    handler.getProcessInput.flush()
    handler.destroyProcess()
    exitCode
  }

  private def runProcess(
    runConfiguration: RunnerAndConfigurationSettings,
    executorClass: Class[_ <: Executor],
    runner: ProgramRunner[_ <: RunnerSettings],
    listeners: Seq[ProcessListener],
  ): (ProcessHandler, RunContentDescriptor) = {
    val executionEnvironment = {
      val configuration = runConfiguration.getConfiguration
      val executor: Executor = Executor.EXECUTOR_EXTENSION_NAME.findExtension(executorClass)
      val builder = new ExecutionEnvironmentBuilder(configuration.getProject, executor)
      builder.runProfile(configuration)
      builder.build()
    }

    val processHandler: AtomicReference[ProcessHandler] = new AtomicReference[ProcessHandler]
    val contentDescriptor: AtomicReference[RunContentDescriptor] = new AtomicReference[RunContentDescriptor]

    val semaphore = new Semaphore(1)

    executionEnvironment.setCallback { (descriptor: RunContentDescriptor) =>
      System.setProperty("idea.dynamic.classpath", useDynamicClassPath.toString)
      val handler: ProcessHandler = descriptor.getProcessHandler
      assertNotNull(handler)
      disposeOnTearDown(new Disposable {
        override def dispose(): Unit = {
          if (!handler.isProcessTerminated)
            handler.destroyProcess()
          descriptor.dispose()
        }
      })
      listeners.foreach(handler.addProcessListener)

      processHandler.set(handler)
      contentDescriptor.set(descriptor)

      semaphore.up()
    }

    runner.execute(executionEnvironment)

    semaphore.waitFor()

    (processHandler.get, contentDescriptor.get)
  }
}

object ScalaTestingTestCase {
  def getScalaTestTemplateConfig(project: Project): ScalaTestRunConfiguration =
    ConfigurationType.CONFIGURATION_TYPE_EP.getExtensions.find(_.getId == "ScalaTestRunConfiguration").
      map(_.getConfigurationFactories.head.createTemplateConfiguration(project)).get.asInstanceOf[ScalaTestRunConfiguration]

  def getSpecs2TemplateConfig(project: Project): Specs2RunConfiguration =
    ConfigurationType.CONFIGURATION_TYPE_EP.getExtensions.find(_.getId == "Specs2RunConfiguration").
      map(_.getConfigurationFactories.head.createTemplateConfiguration(project)).get.asInstanceOf[Specs2RunConfiguration]

  def getUTestTemplateConfig(project: Project): UTestRunConfiguration =
    ConfigurationType.CONFIGURATION_TYPE_EP.getExtensions.find(_.getId == "uTestRunConfiguration").
      map(_.getConfigurationFactories.head.createTemplateConfiguration(project)).get.asInstanceOf[UTestRunConfiguration]

}
