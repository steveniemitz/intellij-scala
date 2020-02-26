package org.jetbrains.plugins.scala
package codeInspection.redundantBlock

import com.intellij.codeInspection.ProblemsHolder
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.parentheses.registerRedundantParensProblem
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, AbstractInspection, ScalaInspectionBundle}
import org.jetbrains.plugins.scala.extensions.childOf
import org.jetbrains.plugins.scala.lang.psi.api.base.ScInterpolatedStringLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScCaseClause, ScCaseClauses}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.refactoring.ScalaNamesValidator.isIdentifier

/**
 * Pavel Fatin
 */

class RedundantBlockInspection extends AbstractInspection {

  override def actionFor(implicit holder: ProblemsHolder, isOnTheFly: Boolean): PartialFunction[PsiElement, Unit] = {
    case (block: ScBlock) childOf ((blockOfExpr: ScBlock) childOf (_: ScCaseClause))
      if block.hasRBrace && block.getFirstChild.textMatches("{") &&
        blockOfExpr.getChildren.length == 1 && !block.getChildren.exists(_.isInstanceOf[ScCaseClauses]) =>
      holder.registerProblem(block, new TextRange(0, 1), ScalaInspectionBundle.message("remove.redundant.braces"), new InCaseClauseQuickFix(block))
    case block: ScBlockExpr if block.getChildren.length == 3 =>
      if (RedundantBlockInspection.isRedundantBlock(block)) {
        registerRedundantParensProblem(ScalaInspectionBundle.message("the.enclosing.block.is.redundant"), block, new QuickFix(block))
      }
  }

  private class QuickFix(e: PsiElement) extends AbstractFixOnPsiElement(ScalaInspectionBundle.message("unwrap.the.expression"), e) {

    override protected def doApplyFix(elem: PsiElement)
                                     (implicit project: Project): Unit = {
      elem.replace(elem.getChildren.apply(1))
    }
  }

  private class InCaseClauseQuickFix(block: ScBlock) extends AbstractFixOnPsiElement(ScalaInspectionBundle.message("remove.redundant.braces"), block) {

    override protected def doApplyFix(bl: ScBlock)
                                     (implicit project: Project): Unit = {
      val children = bl.getChildren.drop(1).dropRight(1)
      for (child <- children) {
        bl.getParent.addBefore(child, bl)
      }
      bl.delete()
    }
  }
}

object RedundantBlockInspection {
  def isRedundantBlock(block: ScBlock): Boolean = {
    val child: PsiElement = block.getChildren.apply(1)
    val probablyRedundant = child match {
      case ref: ScReferenceExpression if ref.qualifier.isEmpty => true
      case t: ScThisReference if t.reference.isEmpty => true
      case _ => false
    }
    if (probablyRedundant) {
      val next: PsiElement = block.getNextSibling
      val parent = block.getParent
      parent match {
        case _: ScArgumentExprList => false
        case _ if next == null => true
        case _: ScInterpolatedStringLiteral =>
          val text = child.getText
          val nextLetter = next.getText.headOption.getOrElse(' ')
          val checkId = isIdentifier(text) && (nextLetter == '$' || !isIdentifier(text + nextLetter))
          checkId && !text.startsWith("_") && !text.exists(_ == '$') && !text.startsWith("`")
        case _ => false
      }
    } else false
  }
}
