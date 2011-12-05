package org.jetbrains.plugins.scala
package lang
package scaladoc
package psi
package impl


import _root_.org.jetbrains.plugins.scala.lang.psi.ScalaPsiElementImpl
import com.intellij.lang.ASTNode
import lang.psi.api.ScalaElementVisitor
import java.lang.String
import com.intellij.psi.{PsiElement, PsiElementVisitor}
import com.intellij.psi.javadoc.{PsiDocComment, PsiDocTagValue}
import lexer.ScalaDocTokenType
import api.{ScDocReferenceElement, ScDocTagValue, ScDocTag}
import lang.psi.impl.ScalaPsiElementFactory

/**
 * User: Alexander Podkhalyuzin
 * Date: 22.07.2008
 */
 
class ScDocTagImpl(node: ASTNode) extends ScalaPsiElementImpl(node) with ScDocTag{
  override def toString: String = "DocTag"

  override def accept(visitor: PsiElementVisitor) {
    visitor match {
      case s: ScalaElementVisitor => accept(s)
      case _ => super.accept(visitor)
    }
  }

  override def accept(visitor: ScalaElementVisitor) {
    visitor.visitTag(this)
  }

  def getContainingComment: PsiDocComment =
    if (getParent.isInstanceOf[PsiDocComment]) getParent.asInstanceOf[PsiDocComment] else null

  def getNameElement: PsiElement = this

  def getDataElements: Array[PsiElement] = getChildren

  def getValueElement: PsiDocTagValue =
    findChildByClass(classOf[PsiDocTagValue])
  
  override def getName: String =
    if (findChildByType(ScalaDocTokenType.DOC_TAG_NAME) != null) {
      findChildByType(ScalaDocTokenType.DOC_TAG_NAME).getText
    } else {
      null
    }

  def setName(name: String): PsiElement = {
    if (findChildByType(ScalaDocTokenType.DOC_TAG_NAME) != null) {
      findChildByType(ScalaDocTokenType.DOC_TAG_NAME).replace(ScalaPsiElementFactory.createDocTagName(name, getManager))
    }

    this
  }
}