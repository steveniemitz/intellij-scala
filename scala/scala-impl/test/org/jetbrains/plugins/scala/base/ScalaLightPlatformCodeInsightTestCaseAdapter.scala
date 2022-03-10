package org.jetbrains.plugins.scala.base

import com.intellij.openapi.editor.Document
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.{ProjectJdkTable, Sdk}
import com.intellij.openapi.util.registry.Registry
import com.intellij.openapi.util.text.StringUtil
import com.intellij.testFramework.{LightPlatformCodeInsightTestCase, LightProjectDescriptor}
import org.jetbrains.plugins.scala.base.libraryLoaders._
import org.jetbrains.plugins.scala.extensions.inWriteAction
import org.jetbrains.plugins.scala.util.TestUtils

/**
 * @author Alexander Podkhalyuzin
 * @deprecated use [[org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter]] instead
 */
@deprecated
abstract class ScalaLightPlatformCodeInsightTestCaseAdapter extends LightPlatformCodeInsightTestCase with ScalaSdkOwner {

  protected def sourceRootPath: String = null

  final protected def baseRootPath: String = TestUtils.getTestDataPath + "/"

  override protected def getProjectJDK: Sdk = SmartJDKLoader.getOrCreateJDK()

  protected def sharedProjectToken: SharedTestProjectToken = SharedTestProjectToken.DoNotShare

  override protected def librariesLoaders: Seq[LibraryLoader] = {
    val builder = Seq.newBuilder[LibraryLoader]
    val scalaLoader = new ScalaSDKLoader(isIncludeReflectLibrary)
    builder += scalaLoader
    builder ++= Option(sourceRootPath).map(SourcesLoader)
    builder ++= additionalLibraries
    builder.result()
  }

  // TODO: can we reuse the project between test cases in an isolated class in ScalaLightPlatformCodeInsightTestCaseAdapter inheritors?
  override protected def getProjectDescriptor: LightProjectDescriptor = new ScalaLightProjectDescriptor(sharedProjectToken) {
    override def tuneModule(module: Module): Unit = afterSetUpProject(module)

    override def getSdk: Sdk = ScalaLightPlatformCodeInsightTestCaseAdapter.this.getProjectJDK
  }

  protected def afterSetUpProject(module: Module): Unit = {
    Registry.get("ast.loading.filter").setValue(true, getTestRootDisposable)
    setUpLibraries(module)
  }

  @throws(classOf[Exception])
  override protected def setUp(): Unit = {
    super.setUp()
    TestUtils.disableTimerThread()
  }

  protected def isIncludeReflectLibrary = false

  protected def additionalLibraries: Seq[LibraryLoader] = Vector.empty

  ////////////////////////////////////////////////////////
  //Some protected methods inherited from Java require overriding + delegating if we want to use them in traits
  //This is required to avoid compilation errors due to some implementation restrictions.
  //For details see https://stackoverflow.com/questions/17557057/how-to-solve-implementation-restriction-trait-accesses-protected-method
  ////////////////////////////////////////////////////////
  override protected def getProject: Project = super.getProject

  override protected def configureFromFileText(fileName: String, fileText: String): Document =
    super.configureFromFileText(fileName, StringUtil.convertLineSeparators(fileText))

  @throws(classOf[Exception])
  override protected def tearDown(): Unit = try {
    disposeLibraries(getModule)
    inWriteAction(ProjectJdkTable.getInstance().removeJdk(getProjectJDK))
  } finally {
    super.tearDown()
  }
}