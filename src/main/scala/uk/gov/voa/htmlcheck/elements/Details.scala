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
import uk.gov.voa.htmlcheck.{ElementOfWrongType, HtmlCheckError}

import scala.language.implicitConversions

case class Details(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Details {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Details =
    if (element.tagName() != "details")
      Left(ElementOfWrongType("details", element.tagName(), IdAttribute(element)))
    else
      Right(Details(element))
}

case class Summary(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement

object Summary {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor Summary =
    if (element.tagName() != "summary")
      Left(ElementOfWrongType("summary", element.tagName(), IdAttribute(element)))
    else
      Right(Summary(element))
}
