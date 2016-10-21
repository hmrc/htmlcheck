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
import uk.gov.voa.htmlcheck.elements.InputRadio.CheckedAttribute
import uk.gov.voa.htmlcheck.{ElementOfWrongType, HtmlCheckError}

import scala.language.implicitConversions

case class InputRadio(protected val element: Element) extends Input {

  lazy val checked: Boolean = CheckedAttribute(element).isDefined
}

object InputRadio {

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

  implicit def elementWrapper(element: Element): HtmlCheckError Xor InputRadio =
    if (element.tagName() != "input" || !TypeAttribute(element).exists(_.value == "radio"))
      Left(ElementOfWrongType("input-radio", s"${element.tagName()}${TypeAttribute(element).map(t => s"-$t").getOrElse("")}", IdAttribute(element)))
    else
      Right(InputRadio(element))

}
