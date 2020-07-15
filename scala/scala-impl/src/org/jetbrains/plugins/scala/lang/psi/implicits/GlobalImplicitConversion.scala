package org.jetbrains.plugins.scala
package lang
package psi
package implicits

import com.intellij.openapi.project.Project
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.plugins.scala.lang.completion.ScalaCompletionUtil.findInheritorObjectsForOwner
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.stubs.index.ImplicitConversionIndex
import org.jetbrains.plugins.scala.util.CommonQualifiedNames.PredefFqn

final class GlobalImplicitConversion(owner: ScTypedDefinition,
                                     pathToOwner: String,
                                     val function: ScFunction)
  extends GlobalInstance(owner, pathToOwner, function)

object GlobalImplicitConversion {

  def apply(owner: ScTypedDefinition, pathToOwner: String, function: ScFunction): GlobalImplicitConversion =
    new GlobalImplicitConversion(owner, pathToOwner, function)

  def apply(owner: ScObject, function: ScFunction): GlobalImplicitConversion =
    GlobalImplicitConversion(owner, owner.qualifiedName, function)

  private[implicits] type ImplicitConversionMap = Map[GlobalImplicitConversion, ImplicitConversionData]

  private[implicits] def computeImplicitConversionMap(scope: GlobalSearchScope)
                                                     (implicit project: Project): ImplicitConversionMap =
    (for {
      globalConversion <- collectConversionsIn(scope)
      data <- ImplicitConversionData(globalConversion)
    } yield globalConversion -> data)
      .toMap

  private[this] def collectConversionsIn(scope: GlobalSearchScope)
                                        (implicit project: Project) =
    for {
      function         <- ImplicitConversionIndex.allConversions(scope)
      containingObject <- findInheritorObjectsForOwner(function)
      if containingObject.qualifiedName != PredefFqn
    } yield GlobalImplicitConversion(containingObject, function)

}