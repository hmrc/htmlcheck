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
import org.jsoup.nodes.Element
import uk.gov.voa.htmlcheck.Html._
import uk.gov.voa.htmlcheck._

import scala.collection.JavaConversions._

trait HtmlElement {
  protected def element: Element
}

trait ElementProperties {

  self: HtmlElement =>

  lazy val id: Option[ElementId] = ElementId(element)

  def nextSibling[T <: HtmlElement](implicit elementWrapper: Element => HtmlCheckError Xor T,
                                    manifest: Manifest[T]): HtmlCheckError Xor T =
    Xor.fromOption(Option(element.nextElementSibling), ElementSiblingNotFound(element.id))
      .flatMap(elementWrapper)

  override def toString = ElementId(element).map(_.toString)
    .orElse(Option(element.tagName))
    .getOrElse(getClass.getSimpleName)
}

trait ContainerElement {

  self: HtmlElement =>

  def firstChild[T <: HtmlElement](implicit elementWrapper: Element => HtmlCheckError Xor T,
                                   manifest: Manifest[T]): HtmlCheckError Xor T =
    Xor.fromOption(Option(element.children().first()), ElementOfTypeNotFound(getTagTypeFromManifest, Some("as first child")))
      .flatMap(elementWrapper)

  def findChildById[T <: HtmlElement](id: String)(implicit elementWrapper: Element => HtmlCheckError Xor T,
                                                  manifest: Manifest[T]): HtmlCheckError Xor T =
    Xor.fromOption(Option(element.getElementById(id)), ElementWithIdNotFound(id))
      .flatMap(elementWrapper)

  def findFirstChildOfType[T <: HtmlElement](implicit elementWrapper: Element => HtmlCheckError Xor T,
                                             manifest: Manifest[T]): HtmlCheckError Xor T =
    findChildrenOfType map (_.head)

  def findChildrenOfType[T <: HtmlElement](implicit elementWrapper: Element => HtmlCheckError Xor T,
                                           manifest: Manifest[T]): HtmlCheckError Xor Seq[T] =
    element.children().iterator().toSeq
      .map(elementWrapper)
      .filter(errorOrElement => errorOrElement.isRight) match {
      case Nil => Left(ElementOfTypeNotFound(getTagTypeFromManifest))
      case xorsWithoutErrors => Right(xorsWithoutErrors.foldLeft(Seq.empty[T])((foundChildren, item) => foundChildren :+ item.getOrError))
    }

  def findChildOfTypeByIndex[T <: HtmlElement](index: Int)(implicit elementWrapper: Element => HtmlCheckError Xor T,
                                                           manifest: Manifest[T]): HtmlCheckError Xor T =
    findChildrenOfType flatMap {
      case children if children.size > index => Right(children(index))
      case children => Left(ElementOutOfBounds(getTagTypeFromManifest, children.size, index))
    }

  private def getTagTypeFromManifest(implicit manifest: Manifest[_]) =
    manifest.runtimeClass.getSimpleName.toLowerCase
}
