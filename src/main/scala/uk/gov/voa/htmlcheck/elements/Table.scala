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

case class Table(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Table {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Table =
    if (element.tagName() != "table")
      Left(ElementWithIdOfWrongType(IdAttribute(element), "table", element.tagName()))
    else
      Right(Table(element))
}

case class Caption(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Caption {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Caption =
    if (element.tagName() != "caption")
      Left(ElementWithIdOfWrongType(IdAttribute(element), "caption", element.tagName()))
    else
      Right(Caption(element))
}

case class Colgroup(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Colgroup {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Colgroup =
    if (element.tagName() != "colgroup")
      Left(ElementWithIdOfWrongType(IdAttribute(element), "colgroup", element.tagName()))
    else
      Right(Colgroup(element))
}

case class Col(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Col {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Col =
    if (element.tagName() != "col")
      Left(ElementWithIdOfWrongType(IdAttribute(element), "col", element.tagName()))
    else
      Right(Col(element))
}

case class Thead(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Thead {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Thead =
    if (element.tagName() != "thead")
      Left(ElementWithIdOfWrongType(IdAttribute(element), "thead", element.tagName()))
    else
      Right(Thead(element))
}

case class Tbody(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Tbody {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Tbody =
    if (element.tagName() != "tbody")
      Left(ElementWithIdOfWrongType(IdAttribute(element), "tbody", element.tagName()))
    else
      Right(Tbody(element))
}

case class Tfoot(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Tfoot {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Tfoot =
    if (element.tagName() != "tfoot")
      Left(ElementWithIdOfWrongType(IdAttribute(element), "tfoot", element.tagName()))
    else
      Right(Tfoot(element))
}

case class Tr(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Tr {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Tr =
    if (element.tagName() != "tr")
      Left(ElementWithIdOfWrongType(IdAttribute(element), "tr", element.tagName()))
    else
      Right(Tr(element))
}

case class Th(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Th {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Th =
    if (element.tagName() != "th")
      Left(ElementWithIdOfWrongType(IdAttribute(element), "th", element.tagName()))
    else
      Right(Th(element))
}

case class Td(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Td {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Td =
    if (element.tagName() != "td")
      Left(ElementWithIdOfWrongType(IdAttribute(element), "td", element.tagName()))
    else
      Right(Td(element))
}
