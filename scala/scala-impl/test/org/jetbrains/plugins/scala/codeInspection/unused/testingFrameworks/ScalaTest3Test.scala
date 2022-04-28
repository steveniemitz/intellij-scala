package org.jetbrains.plugins.scala.codeInspection.unused.testingFrameworks

import org.jetbrains.plugins.scala.DependencyManagerBase.RichStr
import org.jetbrains.plugins.scala.base.LibrariesOwner
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.codeInspection.unused.ScalaUnusedDeclarationInspectionTestBase

class ScalaTest3Test extends ScalaUnusedDeclarationInspectionTestBase with LibrariesOwner {

  override protected def librariesLoaders: Seq[LibraryLoader] = Seq(
    IvyManagedLoader(("org.scalatest" %% "scalatest" % "3.1.1").transitive())
  )

  def testScalaTest3TestSuiteEntryPoint(): Unit = checkTextHasNoErrors(
    s"""
       |import org.scalatest.TestSuite
       |class Foo extends TestSuite {}
       |""".stripMargin
  )

  def test_any_flat_spec_methods(): Unit = checkTextHasNoErrors(
    s"""
       |import org.scalatest.flatspec.AnyFlatSpec
       |class Foo extends AnyFlatSpec {
       |  behavior of "An empty Set"
       |
       |  it should "have size 0" in {
       |    assert(Set.empty.size === 0)
       |  }
       |
       |  it should "produce NoSuchElementException when head is invoked" in {
       |    assertThrows[NoSuchElementException] {
       |      Set.empty.head
       |    }
       |  }
       |}
       |""".stripMargin
  )

    def test_ref_spec(): Unit = throw new Exception

}
