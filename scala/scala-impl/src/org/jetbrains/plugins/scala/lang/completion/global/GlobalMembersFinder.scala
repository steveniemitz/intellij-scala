package org.jetbrains.plugins.scala
package lang
package completion
package global

import com.intellij.codeInsight.CodeInsightSettings
import com.intellij.codeInsight.completion.{InsertHandler, JavaCompletionFeatures, JavaCompletionUtil}
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.featureStatistics.FeatureUsageTracker
import com.intellij.psi.{PsiClass, PsiMember, PsiNamedElement}
import com.intellij.util.ThreeState
import org.jetbrains.plugins.scala.extensions.{PsiClassExt, PsiNamedElementExt}
import org.jetbrains.plugins.scala.lang.completion.lookups.ScalaLookupItem
import org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult

abstract class GlobalMembersFinder protected(protected val place: ScalaPsiElement,
                                             protected val accessAll: Boolean) {

  import GlobalMembersFinder._

  protected trait GlobalMemberInsertHandler {

    this: InsertHandler[LookupElement] =>

    def triggerGlobalMemberCompletionFeature(): Unit =
      FeatureUsageTracker
        .getInstance
        .triggerFeatureUsed(JavaCompletionFeatures.GLOBAL_MEMBER_NAME)
  }

  protected final def isAccessible(member: PsiMember): Boolean =
    accessAll ||
      completion.isAccessible(member)(place)

  final def lookupItems(reference: ScReferenceExpression): Iterable[LookupElement] = {
    val shouldImport = new ShouldImportPredicate(reference)
    candidates
      .map(_.createLookupItem(shouldImport))
      .filterNot(_ == null)
  }

  protected def candidates: Iterable[GlobalMemberResult]

  protected abstract class GlobalMemberResult(protected val resolveResult: ScalaResolveResult,
                                              protected val classToImport: PsiClass,
                                              containingClass: Option[PsiClass] = None) {

    final def createLookupItem(shouldImport: PsiNamedElement => ThreeState): LookupElement =
      if (isApplicable)
        resolveResult.getLookupElement(
          isClassName = true,
          containingClass = containingClass
        ) match {
          case Some(lookupItem) =>
            buildItem(
              lookupItem,
              shouldImport(lookupItem.getPsiElement)
            )
          case _ => null
        }
      else
        null

    protected def buildItem(lookupItem: ScalaLookupItem,
                            shouldImport: ThreeState): LookupElement = {
      lookupItem.shouldImport = shouldImport != ThreeState.NO
      lookupItem.setInsertHandler(createInsertHandler(shouldImport))
      lookupItem.withBooleanUserData(JavaCompletionUtil.FORCE_SHOW_SIGNATURE_ATTR)
    }

    protected def createInsertHandler(shouldImport: ThreeState): InsertHandler[LookupElement]

    private def isApplicable: Boolean = Option(classToImport.qualifiedName).forall(isNotExcluded)
  }
}

object GlobalMembersFinder {

  private final class ShouldImportPredicate(reference: ScReferenceExpression)
    extends (PsiNamedElement => ThreeState) {

    import ThreeState._

    private lazy val elements = reference
      .completionVariants()
      .toSet[ScalaLookupItem]
      .map(_.getPsiElement)

    override def apply(element: PsiNamedElement): ThreeState =
      if (elements.contains(element)) NO
      else if (elements.exists(_.name == element.name)) YES
      else UNSURE
  }

  private def isNotExcluded(qualifiedName: String): Boolean = {
    CodeInsightSettings.getInstance.EXCLUDED_PACKAGES.forall { excludedPackage =>
      qualifiedName != excludedPackage && !qualifiedName.startsWith(excludedPackage + ".")
    }
  }
}