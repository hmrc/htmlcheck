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
import uk.gov.voa.htmlcheck.elements.A.HrefAttribute
import uk.gov.voa.htmlcheck.elements.ElementAttribute.IdAttribute
import uk.gov.voa.htmlcheck.{AttributeNotFound, ElementOfWrongType, HtmlCheckError}

import scala.language.implicitConversions

case class A(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement {

  lazy val href = HrefAttribute(element)
}

object A {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor A =
    if (element.tagName() != "a")
      Left(ElementOfWrongType("a", element.tagName(), IdAttribute(element)))
    else
      Right(A(element))

  case class HrefAttribute(value: String) extends ElementAttributeWithValue

  object HrefAttribute {

    def apply(element: Element): HtmlCheckError Xor HrefAttribute =
      element.attr("href") match {
        case "" => Left(AttributeNotFound(AttributeName("href")))
        case value => Right(HrefAttribute(value))
      }
  }

}
