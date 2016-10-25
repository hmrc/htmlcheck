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
import uk.gov.voa.htmlcheck.elements.ElementAttribute.IdAttribute
import uk.gov.voa.htmlcheck.elements.Form.{ActionAttribute, MethodAttribute}
import uk.gov.voa.htmlcheck.{AttributeNotFound, ElementOfWrongType, HtmlCheckError}

import scala.language.implicitConversions

case class Form(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement {

  lazy val action: HtmlCheckError Xor ActionAttribute = ActionAttribute(element)
  lazy val method: MethodAttribute = MethodAttribute(element)
}

object Form {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Form =
    if (element.tagName() != "form")
      Left(ElementOfWrongType("form", element.tagName(), IdAttribute(element)))
    else
      Right(Form(element))

  case class ActionAttribute(value: String) extends ElementAttribute

  object ActionAttribute {

    def apply(element: Element): HtmlCheckError Xor ActionAttribute =
      element.attr("action") match {
        case "" => Left(AttributeNotFound(AttributeName("action")))
        case value => Right(ActionAttribute(value))
      }
  }

  trait MethodAttribute extends ElementAttribute

  object MethodAttribute {

    case object Post extends MethodAttribute {
      lazy val value = "post"
    }

    case object Get extends MethodAttribute {
      lazy val value = "get"
    }

    def apply(element: Element): MethodAttribute =
      element.attr("method").toLowerCase match {
        case "" => Get
        case Post.value => Post
        case Get.value => Get
      }
  }

}
