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
import cats.data.Xor.{Left, Right}
import org.jsoup.nodes.Element
import uk.gov.voa.htmlcheck.elements.ElementAttribute.{IdAttribute, ValueAttribute}
import uk.gov.voa.htmlcheck.elements.Label.ForAttribute
import uk.gov.voa.htmlcheck.{ElementOfWrongType, HtmlCheckError}

import scala.language.implicitConversions

trait Input
  extends HtmlElement
    with ElementProperties
    with ContainerElement {

  lazy val value: Option[ValueAttribute] = ValueAttribute(element)
}

case class Label(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement {

  lazy val forAttribute = ForAttribute(element)
}

object Label {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Label =
    if (element.tagName() != "label")
      Left(ElementOfWrongType("label", element.tagName(), IdAttribute(element)))
    else
      Right(Label(element))

  case class ForAttribute(value: String) extends ElementAttribute

  object ForAttribute {

    def apply(element: Element): Option[ForAttribute] =
      element.attr("for") match {
        case "" => None
        case value => Some(ForAttribute(value))
      }
  }

}
