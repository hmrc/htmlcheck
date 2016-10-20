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
import uk.gov.voa.htmlcheck.elements.ElementAttribute._
import uk.gov.voa.htmlcheck.elements.Radio.CheckedAttribute
import uk.gov.voa.htmlcheck.{ElementWithIdOfWrongType, HtmlCheckError}

import scala.language.implicitConversions

case class Radio(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement {

  lazy val checked: Boolean = CheckedAttribute(element).isDefined
}

object Radio {

  private case object CheckedAttribute extends ElementAttribute {

    lazy val value = "checked"
    type CheckedAttribute = CheckedAttribute.type

    def apply(element: Element): Option[CheckedAttribute] =
      element.hasAttr("checked") match {
        case false => None
        case true => element.attr("checked") match {
          case selected if selected == value => Some(CheckedAttribute)
          case "" => Some(CheckedAttribute)
          case _ => None
        }
      }
  }

  implicit def selectElementWrapper(element: Element): HtmlCheckError Xor Radio =
    if (element.tagName() != "radio")
      Left(ElementWithIdOfWrongType(IdAttribute(element), "radio", element.tagName()))
    else
      Right(Radio(element))

}
