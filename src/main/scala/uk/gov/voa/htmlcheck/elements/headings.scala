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
import uk.gov.voa.htmlcheck.{ElementWithIdOfWrongType, HtmlCheckError}

import scala.language.implicitConversions

case class H1(elementId: Option[ElementId])(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object H1 {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor H1 =
    if (element.tagName() != "h1")
      Left(ElementWithIdOfWrongType(ElementId(element), "h1", element.tagName()))
    else
      Right(H1(ElementId(element))(element))
}

case class H2(elementId: Option[ElementId])(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object H2 {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor H2 =
    if (element.tagName() != "h2")
      Left(ElementWithIdOfWrongType(ElementId(element), "h2", element.tagName()))
    else
      Right(H2(ElementId(element))(element))
}
