package org.jetbrains.plugins.scala
package lang
package completion3

import org.jetbrains.plugins.scala.base.SharedTestProjectToken

class Scala3KeywordCompletionTest extends ScalaCodeInsightTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean =
    version >= LatestScalaVersions.Scala_3_0

  override def sharedProjectToken: SharedTestProjectToken = SharedTestProjectToken(this.getClass)

  /// INFIX

  def testInfixTopLevel(): Unit = doCompletionTest(
    fileText = s"in$CARET",
    resultText = s"infix $CARET",
    item = "infix"
  )

  def testInfixInsideObject(): Unit = doCompletionTest(
    fileText =
      s"""object O:
         |  in$CARET
         |""".stripMargin,
    resultText =
      s"""object O:
         |  infix $CARET
         |""".stripMargin,
    item = "infix"
  )

  def testSoftModifierAfterInfix(): Unit = doCompletionTest(
    fileText = s"infix in$CARET",
    resultText = s"infix inline $CARET",
    item = "inline"
  )

  def testInfixAfterHardModifier(): Unit = doCompletionTest(
    fileText = s"private in$CARET",
    resultText = s"private infix $CARET",
    item = "infix"
  )

  def testHardModifierAfterInfix(): Unit = doCompletionTest(
    fileText = s"infix pr$CARET",
    resultText = s"infix private $CARET",
    item = "private"
  )

  def testInfixDef(): Unit = doCompletionTest(
    fileText = s"infix d$CARET",
    resultText = s"infix def $CARET",
    item = "def"
  )

  def testInfixType(): Unit = doCompletionTest(
    fileText = s"infix t$CARET",
    resultText = s"infix type $CARET",
    item = "type"
  )

  /// INLINE

  def testInlineTopLevel(): Unit = doCompletionTest(
    fileText = s"in$CARET",
    resultText = s"inline $CARET",
    item = "inline"
  )

  def testInlineInsideObject(): Unit = doCompletionTest(
    fileText =
      s"""object O:
         |  in$CARET
         |""".stripMargin,
    resultText =
      s"""object O:
         |  inline $CARET
         |""".stripMargin,
    item = "inline"
  )

  def testSoftModifierAfterInline(): Unit = doCompletionTest(
    fileText = s"inline tr$CARET",
    resultText = s"inline transparent $CARET",
    item = "transparent"
  )

  def testInlineAfterHardModifier(): Unit = doCompletionTest(
    fileText = s"private in$CARET",
    resultText = s"private inline $CARET",
    item = "inline"
  )

  def testHardModifierAfterInline(): Unit = doCompletionTest(
    fileText = s"inline pr$CARET",
    resultText = s"inline private $CARET",
    item = "private"
  )

  def testInlineDef(): Unit = doCompletionTest(
    fileText = s"infix d$CARET",
    resultText = s"infix def $CARET",
    item = "def"
  )

  def testInlineVal(): Unit = doCompletionTest(
    fileText = s"inline v$CARET",
    resultText = s"inline val $CARET",
    item = "val"
  )

  def testInlineParamOfInlineDef(): Unit = doCompletionTest(
    fileText = s"inline def foo($CARET)",
    resultText = s"inline def foo(inline $CARET)",
    item = "inline"
  )

  def testNoCompletionInlineParamOfDef(): Unit = checkNoBasicCompletion(
    fileText = s"def foo($CARET)",
    item = "inline"
  )

  def testInlineBodyOfInlineDef(): Unit = doCompletionTest(
    fileText = s"inline def foo() = $CARET",
    resultText = s"inline def foo() = inline $CARET",
    item = "inline"
  )

  def testNoCompletionInlineBodyOfDef(): Unit = checkNoBasicCompletion(
    fileText = s"def foo() = $CARET",
    item = "inline"
  )

  /// OPAQUE

  def testOpaqueTopLevel(): Unit = doCompletionTest(
    fileText = s"op$CARET",
    resultText = s"opaque $CARET",
    item = "opaque"
  )

  def testOpaqueInsideObject(): Unit = doCompletionTest(
    fileText =
      s"""object O:
         |  op$CARET
         |""".stripMargin,
    resultText =
      s"""object O:
         |  opaque $CARET
         |""".stripMargin,
    item = "opaque"
  )

  def testSoftModifierAfterOpaque(): Unit = doCompletionTest(
    fileText = s"opaque in$CARET",
    resultText = s"opaque infix $CARET",
    item = "infix"
  )

  def testOpaqueAfterHardModifier(): Unit = doCompletionTest(
    fileText = s"private op$CARET",
    resultText = s"private opaque $CARET",
    item = "opaque"
  )

  def testHardModifierAfterOpaque(): Unit = doCompletionTest(
    fileText = s"opaque pr$CARET",
    resultText = s"opaque private $CARET",
    item = "private"
  )

  def testOpaqueType(): Unit = doCompletionTest(
    fileText = s"opaque t$CARET",
    resultText = s"opaque type $CARET",
    item = "type"
  )

  /// OPEN

  def testOpenTopLevel(): Unit = doCompletionTest(
    fileText = s"op$CARET",
    resultText = s"open $CARET",
    item = "open"
  )

  def testOpenInsideObject(): Unit = doCompletionTest(
    fileText =
      s"""object O:
         |  op$CARET
         |""".stripMargin,
    resultText =
      s"""object O:
         |  open $CARET
         |""".stripMargin,
    item = "open"
  )

  def testSoftModifierAfterOpen(): Unit = doCompletionTest(
    fileText = s"open t$CARET",
    resultText = s"open transparent $CARET",
    item = "transparent"
  )

  def testOpenAfterHardModifier(): Unit = doCompletionTest(
    fileText = s"private op$CARET",
    resultText = s"private open $CARET",
    item = "open"
  )

  def testHardModifierAfterOpen(): Unit = doCompletionTest(
    fileText = s"open ab$CARET",
    resultText = s"open abstract $CARET",
    item = "abstract"
  )

  def testOpenClass(): Unit = doCompletionTest(
    fileText = s"open c$CARET",
    resultText = s"open class $CARET",
    item = "class"
  )

  def testOpenObject(): Unit = doCompletionTest(
    fileText = s"open o$CARET",
    resultText = s"open object $CARET",
    item = "object"
  )

  def testOpenTrait(): Unit = doCompletionTest(
    fileText = s"open t$CARET",
    resultText = s"open trait $CARET",
    item = "trait"
  )

  def testOpenCase(): Unit = doCompletionTest(
    fileText = s"open c$CARET",
    resultText = s"open case $CARET",
    item = "case"
  )

  def testOpenCaseClass(): Unit = doCompletionTest(
    fileText = s"open case c$CARET",
    resultText = s"open case class $CARET",
    item = "class"
  )

  /// TRANSPARENT

  def testTransparentTopLevel(): Unit = doCompletionTest(
    fileText = s"tr$CARET",
    resultText = s"transparent $CARET",
    item = "transparent"
  )

  def testTransparentInsideObject(): Unit = doCompletionTest(
    fileText =
      s"""object O:
         |  tr$CARET
         |""".stripMargin,
    resultText =
      s"""object O:
         |  transparent $CARET
         |""".stripMargin,
    item = "transparent"
  )

  def testSoftModifierAfterTransparent(): Unit = doCompletionTest(
    fileText = s"transparent in$CARET",
    resultText = s"transparent inline $CARET",
    item = "inline"
  )

  def testTransparentAfterHardModifier(): Unit = doCompletionTest(
    fileText = s"private tr$CARET",
    resultText = s"private transparent $CARET",
    item = "transparent"
  )

  def testHardModifierAfterTransparent(): Unit = doCompletionTest(
    fileText = s"transparent pr$CARET",
    resultText = s"transparent private $CARET",
    item = "private"
  )

  def testTransparentDef(): Unit = doCompletionTest(
    fileText = s"transparent d$CARET",
    resultText = s"transparent def $CARET",
    item = "def"
  )

  def testTransparentTrait(): Unit = doCompletionTest(
    fileText = s"transparent t$CARET",
    resultText = s"transparent trait $CARET",
    item = "trait"
  )

}