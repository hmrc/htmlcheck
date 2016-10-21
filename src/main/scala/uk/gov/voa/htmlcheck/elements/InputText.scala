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
import uk.gov.voa.htmlcheck.elements.ElementAttribute.{IdAttribute, TypeAttribute}
import uk.gov.voa.htmlcheck.{ElementOfWrongType, HtmlCheckError}

import scala.language.implicitConversions

case class InputText(protected val element: Element) extends Input

object InputText {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor InputText =
    if (element.tagName() != "input" || !TypeAttribute(element).exists(_.value == "text"))
      Left(ElementOfWrongType("input-text", s"${element.tagName()}${TypeAttribute(element).map(t => s"-$t").getOrElse("")}", IdAttribute(element)))
    else
      Right(InputText(element))

}
