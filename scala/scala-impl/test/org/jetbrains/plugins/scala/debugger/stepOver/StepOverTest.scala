package org.jetbrains.plugins.scala
package debugger
package stepOver

import org.jetbrains.plugins.scala.extensions.inReadAction
import org.junit.Assert
import org.junit.experimental.categories.Category

import scala.io.Source
import scala.util.Using

@Category(Array(classOf[DebuggerTests]))
class StepOverTest_2_11 extends StepOverTest {
  override protected def supportedIn(version: ScalaVersion): Boolean = version == LatestScalaVersions.Scala_2_11
}

@Category(Array(classOf[DebuggerTests]))
class StepOverTest_2_12 extends StepOverTest {
  override protected def supportedIn(version: ScalaVersion): Boolean = version == LatestScalaVersions.Scala_2_12

  override def testPartialFun(): Unit = {
    testStepThrough(Seq(4, 5, 6, 3, 4, 3, 7, 8, 9, 3, 4, 3, 7, 3, 3, 11))
  }

  override def testMultilineExpr(): Unit = {
    testStepThrough(Seq(2, 4, 3, 4, 6, 7, 8, 9, 4))
  }

  override def testCaseClausesReturn(): Unit = {
    testStepThrough(Seq(6, 7, 9, 11, 12, 6, 2))
  }

  override def testComplexPattern(): Unit = {
    testStepThrough(Seq(2, 3, 4, 7, 10, 11, 12, 3, 14))
  }

  override def testSimple(): Unit = {
    testStepThrough(Seq(2, 3, 4, 5, 6, 7, 8, 3, 1))
  }

  override def testNestedMatch(): Unit = {
    testStepThrough(Seq(2, 3, 4, 5, 8, 9, 10, 4, 14))
  }

  override def testAccessorInDelayedInit(): Unit = {
    testStepThrough(Seq(1, 2, 3, 4, 0, 0))
  }
}

@Category(Array(classOf[DebuggerTests]))
class StepOverTest_2_13 extends StepOverTest_2_12 {
  override protected def supportedIn(version: ScalaVersion): Boolean = version == LatestScalaVersions.Scala_2_13

  override def testPartialFun(): Unit = {
    testStepThrough(Seq(4, 5, 6, 3, 4, 7, 8, 9, 3, 4, 7, 3, 11))
  }

  override def testCaseClausesReturn(): Unit = {
    testStepThrough(Seq(6, 11, 6, 12, 6, 2, 12, 6, 2))
  }
}

@Category(Array(classOf[DebuggerTests]))
class StepOverTest_3_0 extends StepOverTest_2_13 {
  override protected def supportedIn(version: ScalaVersion): Boolean = version == LatestScalaVersions.Scala_3_0

  override def testSimple(): Unit = {
    testStepThrough(Seq(2, 3, 4, 5, 6, 8, 10, 1))
  }

  override def testMultilineExpr(): Unit = {
    testStepThrough(Seq(2, 3, 4, 6, 8, 9, 10, 1))
  }

  override def testSkipStoreResult(): Unit = {
    testStepThrough(Seq(2, 3, 4, 5, 6, 9, 11))
  }

  override def testPartialFun(): Unit = {
    testStepThrough(Seq(4, 5, 6, 9, 4, 7, 8, 9, 4, 7, 9, 9, 11))
  }

  override def testNestedMatch(): Unit = {
    testStepThrough(Seq(2, 3, 4, 5, 8, 9, 10, 12, 14))
  }

  override def testCaseClausesReturn(): Unit = {
    testStepThrough(Seq(6, 7, 9, 11, 12, 2, 12, 6, 2))
  }

  override def testComplexPattern(): Unit = {
    testStepThrough(Seq(2, 3, 4, 7, 10, 11, 12, 14))
  }
}

@Category(Array(classOf[DebuggerTests]))
class StepOverTest_3_1 extends StepOverTest_3_0 {
  override protected def supportedIn(version: ScalaVersion): Boolean = version == LatestScalaVersions.Scala_3_1

  override def testCaseClausesReturn(): Unit = {
    testStepThrough(Seq(6, 7, 12, 2, 12, 2, 12, 6, 2))
  }
}

abstract class StepOverTest extends StepOverTestBase {
  addFileWithBreakpoints("Simple.scala",
    s"""
      |object Simple {
      |  def main (args: Array[String]): Unit = {
      |    println()$bp
      |    List(1) match {
      |      case Seq(2) =>
      |      case Seq(3) =>
      |      case IndexedSeq(5) =>
      |      case IndexedSeq(6) =>
      |      case Seq(1) =>
      |      case Seq(7) =>
      |      case Seq(8) =>
      |    }
      |  }
      |}
    """.stripMargin.trim)
  def testSimple(): Unit = {
    testStepThrough(Seq(2, 3, 4, 5, 6, 8, 1))
  }

  addFileWithBreakpoints("MultilineExpr.scala",
    s"""
       |object MultilineExpr {
       |  def main (args: Array[String]): Unit = {
       |    println()$bp
       |    Seq(2, 3)
       |      .map(_ - 1)
       |    match {
       |      case IndexedSeq(1, 2) =>
       |      case IndexedSeq(2, 3) =>
       |      case Seq(2) =>
       |      case Seq(1, _) =>
       |      case Seq(3) =>
       |    }
       |  }
       |}
    """.stripMargin.trim)
  def testMultilineExpr(): Unit = {
    testStepThrough(Seq(2, 3, 4, 6, 8, 9, 1))
  }

  addFileWithBreakpoints("SkipStoreResult.scala",
    s"""
       |object SkipStoreResult {
       |  def main (args: Array[String]): Unit = {
       |    println()$bp
       |    val z = Seq(1, 2) match {
       |      case Seq(1, _) =>
       |        foo()
       |        fee()
       |      case _ =>
       |        fee()
       |        foo()
       |    }
       |    println(z)
       |  }
       |
       |  def foo() = "foo"
       |  def fee() = "fee"
       |}
    """.stripMargin.trim)
  def testSkipStoreResult(): Unit = {
    testStepThrough(Seq(2, 3, 4, 5, 6, 11))
  }

  addFileWithBreakpoints("PartialFun.scala",
    s"""
        |object PartialFun {
        |  def main (args: Array[String]): Unit = {
        |    ""
        |    val z = Seq(Some(1), Some(2), Some(3)) collect {
        |      case Some(1) =>$bp
        |        foo()
        |        fee()
        |      case Some(2) =>
        |        fee()
        |        foo()
        |    }
        |    println(z)
        |  }
        |
        |  def foo() = "foo"
        |  def fee() = "fee"
        |}
    """.stripMargin.trim)
  def testPartialFun(): Unit = {
    testStepThrough(Seq(4, 5, 6, 3, 4, 7, 8, 9, 3, 4, 7, 3, 11))
  }


  addFileWithBreakpoints("ComplexPattern.scala",
    s"""
       |object ComplexPattern {
       |  def main (args: Array[String]): Unit = {
       |    println()$bp
       |    val z = Seq(left(1), left(2)) match {
       |      case Seq(Right("1")) =>
       |        foo()
       |        fee()
       |      case Left(Seq(Some(x))) and Left(Seq(None)) =>
       |        fee()
       |        foo()
       |      case Left(Seq(_)) and Left(Seq(Some(2))) =>
       |        fee()
       |        foo()
       |    }
       |    println(z)
       |  }
       |
       |  def foo() = "foo"
       |  def fee() = "fee"
       |  def left(i: Int): Either[Seq[Option[Int]], String] = Left(Seq(Some(i)))
       |
       |  object and {
       |    def unapply(s: Seq[_]): Option[(Any, Any)] = {
       |      s match {
       |        case Seq(x, y) => Some((x, y))
       |        case _ => None
       |      }
       |    }
       |  }
       |}
       |
    """.stripMargin.trim)
  def testComplexPattern(): Unit = {
    testStepThrough(Seq(2, 3, 4, 7, 10, 11, 12, 14))
  }

  addFileWithBreakpoints("NestedMatch.scala",
    s"""
       |object NestedMatch {
       |  def main (args: Array[String]): Unit = {
       |    println()$bp
       |    val z = Seq(left(1), left(2)) match {
       |      case Seq(Left(Seq(Some(1))), x) => x match {
       |        case Left(Seq(None)) =>
       |          fee()
       |          foo()
       |        case Left(Seq(Some(2))) =>
       |          fee()
       |          foo()
       |      }
       |      case _ =>
       |    }
       |    println(z)
       |  }
       |
       |  def foo() = "foo"
       |  def fee() = "fee"
       |  def left(i: Int): Either[Seq[Option[Int]], String] = Left(Seq(Some(i)))
       |}
    """.stripMargin.trim)
  def testNestedMatch(): Unit = {
    testStepThrough(Seq(2, 3, 4, 5, 8, 9, 10, 14))
  }

  addFileWithBreakpoints("CaseClausesReturn.scala",
    s"""
      |object CaseClausesReturn {
      |  def main(args: Array[String]): Unit = {
      |    foo()
      |  }
      |
      |  def foo() = {
      |    "aaa" match {$bp
      |      case "qwe" =>
      |        println(1)
      |      case "wer" =>
      |        println(2)
      |      case "aaa" =>
      |        println(3)
      |    }
      |  }
      |}
      | """.stripMargin.trim)

  def testCaseClausesReturn(): Unit = {
    testStepThrough(Seq(6, 7, 9, 11, 12, 2))
  }

  addFileWithBreakpoints("AccessorInDelayedInit.scala",
  s"""
     |object AccessorInDelayedInit extends App {
     |  var x = 5 $bp
     |  x = x + 10
     |  x
     |  println(x)
     |}
  """.stripMargin.trim)
  def testAccessorInDelayedInit(): Unit = {
    testStepThrough(Seq(1, 2, 3, 4, 0))
  }
}

abstract class StepOverTestBase extends ScalaDebuggerTestCase {
  def doStepOver(): Unit = {
    val stepOverCommand = getDebugProcess.createStepOverCommand(currentSuspendContext(), false)
    getDebugProcess.getManagerThread.invokeAndWait(stepOverCommand)
  }

  def testStepThrough(expectedLineNumbers: Seq[Int]): Unit = {
    val file = getFileInSrc(mainFileName)
    val lines = Using.resource(Source.fromFile(file))(_.getLines().toSeq)
    Assert.assertTrue(s"File should start with definition of object $mainClassName" , lines.head.startsWith(s"object $mainClassName"))

    def checkLine(expectedLineNumber: Int): Unit = {
      val actualLineNumber = currentLineNumber
      if (actualLineNumber != expectedLineNumber) {
        val message = {
          val actualLine = lines(actualLineNumber)
          val expectedLine = lines(expectedLineNumber)
          s"""Wrong line number.
             |Expected $expectedLineNumber: $expectedLine
             |Actual $actualLineNumber: $actualLine""".stripMargin
        }
        Assert.fail(message)
      }
    }

    val expectedNumbers = expectedLineNumbers.iterator
    runDebugger(mainClassName) {
      while (!processTerminatedNoBreakpoints()) {
        if (expectedNumbers.hasNext) checkLine(expectedNumbers.next())
        else {
          val lineNumber = currentLineNumber
          Assert.fail(s"No expected lines left, stopped at line $lineNumber: ${lines(lineNumber)}")
        }
        doStepOver()
      }
    }
  }

  private def currentLineNumber: Int = {
    val location = currentLocation()
    inReadAction {
      positionManager.getSourcePosition(location).getLine
    }
  }
}