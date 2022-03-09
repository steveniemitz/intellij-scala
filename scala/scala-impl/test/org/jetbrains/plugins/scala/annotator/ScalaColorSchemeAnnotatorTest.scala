package org.jetbrains.plugins.scala.annotator

import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.psi.PsiElement

class ScalaColorSchemeAnnotatorTest extends ScalaColorSchemeAnnotatorTestBase[TextAttributesKey] {
  import org.jetbrains.plugins.scala.highlighter.DefaultHighlighter.{GENERATOR, PATTERN}

  override protected def buildAnnotationsTestText(annotations: Seq[Message2]): String =
    annotations.map(_.textWithoutRangeAndAttributeKey).mkString("\n")

  protected def needToAnnotateElement(element: PsiElement): Boolean = true

  override protected def getFilterByField(annotation: Message2): TextAttributesKey = annotation.textAttributesKey

  def testAnnotateGeneratorAndEnumerator(): Unit = {
    val text =
      s"""for {
         |  case (a, b) <- Seq()
         |  (c, d) <- Seq()
         |} yield {
         |  println((a, b))
         |  println((c, d))
         |}
         |""".stripMargin

    testAnnotations(text, GENERATOR,
      """Info((68,69),Scala For statement value)
        |Info((71,72),Scala For statement value)
        |Info((86,87),Scala For statement value)
        |Info((89,90),Scala For statement value)
        |Info((14,15),Scala For statement value)
        |Info((17,18),Scala For statement value)
        |Info((32,33),Scala For statement value)
        |Info((35,36),Scala For statement value)
        |""".stripMargin)
  }

  def testAnnotatePattern_1(): Unit = {
    val text =
      s"""??? match {
         |  case (a, b) =>
         |    (a, b)
         |}""".stripMargin

    testAnnotations(text, PATTERN,
      """Info((34,35),Scala Pattern value)
        |Info((37,38),Scala Pattern value)
        |Info((20,21),Scala Pattern value)
        |Info((23,24),Scala Pattern value)
        |""".stripMargin)
  }

  def testAnnotatePattern_2(): Unit = {
    val text =
      s"""val sourceRoots = Seq()
         |val translatedTemplatePath = ""
         |
         |lazy val xxx: Option[String] = {
         |  sourceRoots collectFirst {
         |    case root if  root == null =>
         |      println(root)
         |      ???
         |  }
         |}
         |
         |lazy val yyy: Option[String] = {
         |  sourceRoots collectFirst {
         |    case root if root == null =>
         |      println(root)
         |      42 // type mismatch error
         |  }
         |}""".stripMargin

    testAnnotations(text, PATTERN,
      """Info((128,132),Scala Pattern value)
        |Info((261,265),Scala Pattern value)
        |Info((137,141),Scala Pattern value)
        |Info((269,273),Scala Pattern value)
        |Info((167,171),Scala Pattern value)
        |Info((299,303),Scala Pattern value)
        |""".stripMargin)
  }
}
