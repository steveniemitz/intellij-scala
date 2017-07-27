package org.jetbrains.plugins.scala.settings

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScFunctionDefinition, ScPatternDefinition, ScVariableDefinition}
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult

/**
  * @author Pavel Fatin
  */
trait Implementation {
  def isTypeObvious: Boolean
}

object Implementation {
  private val TraversableClassNames =
    Seq("Seq", "Array", "Vector", "Set", "HashSet", "Map", "HashMap", "Iterator", "Option")

  def apply(definition: PsiElement): Implementation = new Definition(definition)

  def of(expression: PsiElement): Implementation = new Expression(expression)

  private class Definition(element: PsiElement) extends Implementation {
    override def isTypeObvious: Boolean = rightHandSideOf(element).exists(isSimple)
  }

  private class Expression(element: PsiElement) extends Implementation {
    override def isTypeObvious: Boolean = isSimple(element)
  }

  private def isSimple(expression: PsiElement): Boolean = expression match {
    case _: ScLiteral => true
    case _: ScNewTemplateDefinition => true
    case ref: ScReferenceExpression if isObject(ref) => true
    case ScGenericCall(referenced, _) if isFactoryMethod(referenced) => true
    case ScMethodCall(invoked: ScReferenceExpression, _) if isObject(invoked) => true
    case _: ScThrowStmt => true
    case _ => false
  }

  private def rightHandSideOf(element: PsiElement) = element match {
    case value: ScPatternDefinition if value.isSimple => value.expr
    case variable: ScVariableDefinition if variable.isSimple => variable.expr
    case method: ScFunctionDefinition if method.hasAssign && !method.isSecondaryConstructor => method.body
    case _ => None //support isSimple for JavaPsi
  }

  private def isObject(reference: ScReferenceExpression): Boolean = {
    def resolvedElement(result: ScalaResolveResult) =
      result.innerResolveResult
        .getOrElse(result).element

    reference.bind().map(resolvedElement).exists {
      case function: ScFunction => function.isApplyMethod
      case _ => false
    }
  }

  def isFactoryMethod(referenced: ScReferenceExpression): Boolean = referenced match {
    case ScReferenceExpression.withQualifier(qualifier: ScReferenceExpression) =>
      TraversableClassNames.contains(qualifier.refName) && referenced.refName == "empty"
    case _ => false
  }
}
