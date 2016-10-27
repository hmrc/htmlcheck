/*
 * Copyright 2016 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.voa.htmlcheck.elements

import cats.data.Xor
import cats.data.Xor._
import cats.kernel.Semigroup
import org.jsoup.nodes.Element
import uk.gov.voa.htmlcheck._
import uk.gov.voa.htmlcheck.elements.ElementAttribute._

import scala.collection.JavaConversions._
import scala.language.implicitConversions

trait HtmlElement {
  protected def element: Element
}

trait ElementProperties {

  self: HtmlElement =>

  lazy val id: HtmlCheckError Xor IdAttribute = IdAttribute(element)

  lazy val name: HtmlCheckError Xor NameAttribute = NameAttribute(element)

  lazy val className: HtmlCheckError Xor ClassAttribute = ClassAttribute(element)

  lazy val classNames: Set[ClassAttribute] = element.classNames().toSet[String].map(ClassAttribute.apply)

  lazy val text: String = element.text()

  def attribute(name: AttributeName): HtmlCheckError Xor GenericAttribute = GenericAttribute(name, element)

  def nextSibling[T <: HtmlElement](implicit elementWrapper: Element => HtmlCheckError Xor T,
                                    manifest: Manifest[T]): HtmlCheckError Xor T =
    Xor.fromOption(Option(element.nextElementSibling), ElementSiblingNotFound(id))
      .flatMap(elementWrapper)

  override def toString = IdAttribute(element).map(_.toString)
    .orElse(Right(element.tagName))
    .getOrElse(getClass.getSimpleName)
}

trait ContainerElement {

  self: HtmlElement =>

  def firstChild[T <: HtmlElement](implicit elementWrapper: Element => HtmlCheckError Xor T,
                                   manifest: Manifest[T]): HtmlCheckError Xor T =
    Xor.fromOption(Option(element.children().first()), ElementOfTypeNotFound(getTagTypeFromManifest, Some("as first child")))
      .flatMap(elementWrapper)

  def onlyChild[T <: HtmlElement](implicit elementWrapper: Element => HtmlCheckError Xor T,
                                  manifest: Manifest[T]): HtmlCheckError Xor T =
    element.children().toSeq match {
      case elements if elements.isEmpty => Left(ElementOfTypeNotFound(getTagTypeFromManifest, Some("as the only child")))
      case elements if elements.size > 1 => Left(MoreThanOneElementFound(elements.size, getTagTypeFromManifest))
      case elements => elementWrapper(elements.head)
    }

  def findOnlyDescendantBy[AT <: ElementAttribute, T <: HtmlElement](attribute: AT)(implicit findDescendantsBy: AT => Element => Seq[Element],
                                                                                    elementWrapper: Element => HtmlCheckError Xor T,
                                                                                    manifest: Manifest[T]): HtmlCheckError Xor T =
    findDescendantsBy(attribute)(element).filter(_ != null) match {
      case elements if elements.isEmpty => Left(NoElementsFound(getTagTypeFromManifest, attribute))
      case elements if elements.size == 1 => elementWrapper(elements.head)
      case elements => Left(MoreThanOneElementFound(elements.size, getTagTypeFromManifest, attribute))
    }

  def findFirstDescendantBy[AT <: ElementAttribute, T <: HtmlElement](attribute: AT)(implicit findDescendantsBy: AT => Element => Seq[Element],
                                                                                     elementWrapper: Element => HtmlCheckError Xor T,
                                                                                     manifest: Manifest[T]): HtmlCheckError Xor T =
    findDescendantsBy(attribute)(element).filter(_ != null) match {
      case elements if elements.isEmpty => Left(NoElementsFound(getTagTypeFromManifest, attribute))
      case elements => elementWrapper(elements.head)
    }

  def findChildrenOfType[T <: HtmlElement](implicit elementWrapper: Element => HtmlCheckError Xor T,
                                           manifest: Manifest[T]): HtmlCheckError Xor Seq[T] = {
    implicit val contextualAttribute = None
    val identityPredicate: Element => Boolean = _ => true

    findChildrenMatching(identityPredicate).convertThemTo[T]
  }

  def findFirstChildOfType[T <: HtmlElement](implicit elementWrapper: Element => HtmlCheckError Xor T,
                                             manifest: Manifest[T]): HtmlCheckError Xor T =
    findChildrenOfType map (_.head)

  def findOnlyChildOfType[T <: HtmlElement](implicit elementWrapper: Element => HtmlCheckError Xor T,
                                            manifest: Manifest[T]): HtmlCheckError Xor T =
    findChildrenOfType flatMap {
      case head :: Nil => Right(head)
      case children => Left(MoreThanOneElementFound(children.size, getTagTypeFromManifest))
    }

  def findChildOfTypeByIndex[T <: HtmlElement](index: Int)(implicit elementWrapper: Element => HtmlCheckError Xor T,
                                                           manifest: Manifest[T]): HtmlCheckError Xor T =
    findChildrenOfType flatMap {
      case children if children.size > index => Right(children(index))
      case children => Left(ElementOutOfBounds(getTagTypeFromManifest, children.size, index))
    }

  def findChildrenBy[AT <: ElementAttribute, T <: HtmlElement](attribute: AT)(implicit filteringPredicate: AT => Element => Boolean,
                                                                              elementWrapper: Element => HtmlCheckError Xor T,
                                                                              manifest: Manifest[T]): HtmlCheckError Xor Seq[T] = {
    implicit val contextualAttribute = Some(attribute)

    findChildrenMatching(filteringPredicate(attribute)).convertThemTo[T]
  }

  def findFirstChildBy[AT <: ElementAttribute, T <: HtmlElement](attribute: AT)
                                                                (implicit filteringPredicate: AT => Element => Boolean,
                                                                 elementWrapper: Element => HtmlCheckError Xor T,
                                                                 manifest: Manifest[T]): HtmlCheckError Xor T =
    findChildrenBy[AT, T](attribute).map(_.head)

  def findOnlyChildBy[AT <: ElementAttribute, T <: HtmlElement](attribute: AT)(implicit filteringPredicate: AT => Element => Boolean,
                                                                               elementWrapper: Element => HtmlCheckError Xor T,
                                                                               manifest: Manifest[T]): HtmlCheckError Xor T =
    findChildrenBy[AT, T](attribute).flatMap {
      case head :: Nil => Right(head)
      case children => Left(MoreThanOneElementFound(children.size, getTagTypeFromManifest, attribute))
    }

  private def findChildrenMatching(predicate: Element => Boolean) =
    element.children.iterator().toSeq
      .filter(predicate)

  private implicit class ElementsOps(elements: Seq[Element]) {

    private implicit def elementXorsSemigroup[T <: HtmlElement] = new Semigroup[Seq[T]] {
      def combine(x: Seq[T], y: Seq[T]): Seq[T] = x ++ y
    }

    def convertThemTo[T <: HtmlElement](implicit elementAttribute: Option[ElementAttribute],
                                        elementWrapper: Element => HtmlCheckError Xor T,
                                        manifest: Manifest[T]): HtmlCheckError Xor Seq[T] =
      elements match {
        case Nil =>
          Left(NoElementsFound(getTagTypeFromManifest, elementAttribute))
        case elmnts => elmnts
          .map(elementWrapper)
          .filter(_.isRight) match {
          case Nil =>
            Left(NoElementsFound(getTagTypeFromManifest, elementAttribute))
          case rightsOfConvertedElements =>
            rightsOfConvertedElements.map(elementIntoSeq).reduce(_ combine _)
        }
      }

    private def elementIntoSeq[T <: HtmlElement](xor: HtmlCheckError Xor T): HtmlCheckError Xor Seq[T] =
      xor.map(element => Seq(element))

  }

  private def getTagTypeFromManifest(implicit manifest: Manifest[_]) =
    manifest.runtimeClass.getSimpleName.toLowerCase

}

object HtmlElement {

  object Implicits extends Implicits

  trait Implicits {

    implicit val idPredicate: IdAttribute => Element => Boolean =
      id => _.id() == id.value

    implicit val classPredicate: ClassAttribute => Element => Boolean =
      className => _.classNames().toSet.contains(className.value)

    implicit val descendantByIdFinder: IdAttribute => Element => Seq[Element] =
      id => element => Seq(element.getElementById(id.value))

    implicit val descendantByClassFinder: ClassAttribute => Element => Seq[Element] =
      className => element => element.getElementsByClass(className.value).iterator().toSeq
  }

}

