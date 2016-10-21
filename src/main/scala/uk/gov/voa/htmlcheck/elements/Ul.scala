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
import uk.gov.voa.htmlcheck.elements.ElementAttribute._
import uk.gov.voa.htmlcheck.{ElementOfWrongType, HtmlCheckError}

import scala.language.implicitConversions

case class Ul(elementId: Option[IdAttribute])
             (protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Ul {

  implicit def ulElementWrapper(element: Element): HtmlCheckError Xor Ul =
    if (element.tagName() != "ul")
      Left(ElementOfWrongType("ul", element.tagName(), IdAttribute(element)))
    else
      Right(Ul(IdAttribute(element))(element))
}

case class Li(elementId: Option[IdAttribute])
             (protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Li {

  implicit def liElementWrapper(element: Element): HtmlCheckError Xor Li =
    if (element.tagName() != "li")
      Left(ElementOfWrongType("li", element.tagName(), IdAttribute(element)))
    else
      Right(Li(IdAttribute(element))(element))
}
