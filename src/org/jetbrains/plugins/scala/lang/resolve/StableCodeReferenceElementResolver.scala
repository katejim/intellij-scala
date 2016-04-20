package org.jetbrains.plugins.scala
package lang
package resolve

import com.intellij.psi.impl.source.resolve.ResolveCache
import org.jetbrains.plugins.scala.lang.psi.api.base.ScStableCodeReferenceElement
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScConstructorPattern, ScInfixPattern, ScInterpolationPattern}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.{ScImportExpr, ScImportSelector}
import org.jetbrains.plugins.scala.lang.psi.types.Compatibility.Expression
import org.jetbrains.plugins.scala.lang.resolve.processor._

class StableCodeReferenceElementResolver(reference: ResolvableStableCodeReferenceElement, shapeResolve: Boolean,
                                          allConstructorResults: Boolean, noConstructorResolve: Boolean)
        extends ResolveCache.PolyVariantResolver[ScStableCodeReferenceElement] {
  def resolve(ref: ScStableCodeReferenceElement, incomplete: Boolean) = {
    import ref.typeSystem
    val kinds = getKindsFor(ref)
    val proc = if (ref.isConstructorReference && !noConstructorResolve) {
      val constr = ref.getConstructor.get
      val typeArgs = constr.typeArgList.map(_.typeArgs).getOrElse(Seq())
      val effectiveArgs = constr.arguments.toList.map(_.exprs.map(new Expression(_))) match {
        case List() => List(List())
        case x => x
      }
      //TODO: probably replace
      new ConstructorResolveProcessor(ref, ref.refName.inName, effectiveArgs, typeArgs, kinds, shapeResolve, allConstructorResults)
    } else ref.getContext match {
      //last ref may import many elements with the same name
      case e: ScImportExpr if e.selectorSet.isEmpty && !e.singleWildcard =>
        //TODO: probably replace
        new CollectAllForImportProcessor(kinds, ref, reference.refName.inName)
      case e: ScImportExpr if e.singleWildcard => new ResolveProcessor(kinds, ref, reference.refName.inName) //TODO: probably replace
      case _: ScImportSelector => new CollectAllForImportProcessor(kinds, ref, reference.refName.inName) //TODO: probably replace
      case constr: ScInterpolationPattern =>
        new ExtractorResolveProcessor(ref, reference.refName.inName, kinds, constr.expectedType) //TODO: probably replace
      case constr: ScConstructorPattern =>
        new ExtractorResolveProcessor(ref, reference.refName.inName, kinds, constr.expectedType) //TODO: probably replace
      case infix: ScInfixPattern => new ExtractorResolveProcessor(ref, reference.refName.inName, kinds, infix.expectedType) //TODO: probably replace
      case _ => new ResolveProcessor(kinds, ref, reference.refName.inName) //TODO: probably replace
    }

    reference.doResolve(ref, proc)
  }

  protected def getKindsFor(ref: ScStableCodeReferenceElement) = ref.getKinds(incomplete = false)
}
