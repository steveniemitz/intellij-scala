package org.jetbrains.plugins.scala.annotator

import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.util.TextRange

import scala.math.Ordered.orderingToOrdered

/**
 * Pavel.Fatin, 18.05.2010
 */

sealed abstract class Message extends Ordered[Message] {
  def element: String
  def message: String

  override def compare(that: Message): Int =
    (this.element, this.message) compare (that.element, that.message)
}
// TODO: move it to Message companion object
case class Info(override val element: String, override val message: String) extends Message
case class Warning(override val element: String, override val message: String) extends Message
case class Error(override val element: String, override val message: String) extends Message
case class Hint(override val element: String, text: String, override val message: String = "", offsetDelta: Int = 0) extends Message


//////////////////////////////////
//
// Practically the ame but with range and text attributes
// NOTE: ideally we should consider unifying all tests to use single version of test message
//
//////////////////////////////////

object Message2 {
  case class Info(override val range: TextRange, override val fileText: String, override val message: String, override val textAttributesKey: TextAttributesKey) extends Message2
  case class Warning(override val range: TextRange, override val fileText: String, override val message: String, override val textAttributesKey: TextAttributesKey) extends Message2
  case class Error(override val range: TextRange, override val fileText: String, override val message: String, override val textAttributesKey: TextAttributesKey) extends Message2


  implicit object TextRangeOrdering extends scala.math.Ordering[TextRange] {
    override def compare(x: TextRange, y: TextRange): Int =
      (x.getStartOffset, x.getEndOffset) compare (y.getStartOffset, y.getEndOffset)
  }
}

sealed abstract class Message2 extends Ordered[Message2] {
  def range: TextRange
  /**
   * contains annotated code, corresponding to [[range]]
   *
   * @note [[range]] and [[fileText]] represent interchangeable information (one can be obtained from another given file text)
   *       But it might be more convenient to heep one or another depending on the test
   */
  def fileText: String
  def message: String
  def textAttributesKey: TextAttributesKey

  override def compare(that: Message2): Int = {
    import org.jetbrains.plugins.scala.annotator.Message2.TextRangeOrdering

    import scala.math.Ordered.orderingToOrdered

    (this.range, this.message) compare (that.range, that.message)
  }

  def textWithoutRangeAndCode: String =
    this.getClass.getSimpleName + s"($range,$message)"

  def textWithoutRangeAndAttributeKey: String =
    this.getClass.getSimpleName + s"($range,${textAttributesKey.getExternalName})"
}