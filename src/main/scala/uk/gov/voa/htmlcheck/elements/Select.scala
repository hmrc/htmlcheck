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

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class Select(options: List[SelectOption],
                  errors: Seq[ErrorElement] = Nil)
                 (protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ErrorElements {

  lazy val selectedOption: Option[SelectOption] = options.find(_.selected)

  lazy val selectedOptionValue: Option[ElementValue] = options.find(_.selected).flatMap(_.value)

}

object Select extends ErrorElementsFinder {

  implicit def selectElementWrapper(element: Element): HtmlCheckError Xor Select =
    if (element.tagName() != "select")
      Left(ElementWithIdOfWrongType(ElementId(element), "select", element.tagName()))
    else
      Right(Select(
        options = element.children().toList.map(child => SelectOption(child)),
        errors = findElementErrors(element)
      )(element))

}

case class SelectOption(id: Option[ElementId],
                        value: Option[ElementValue],
                        text: ElementText,
                        selected: Boolean = false)

object SelectOption {

  def apply(element: Element): SelectOption = SelectOption(
    ElementId(element),
    ElementValue(element),
    ElementText(element),
    selected = element.attr("selected") == "selected"
  )

  def apply(elementId: ElementId, elementValue: ElementValue, elementText: ElementText): SelectOption = SelectOption(
    Some(elementId),
    Some(elementValue),
    elementText
  )

  def apply(elementText: ElementText): SelectOption = SelectOption(
    id = None,
    value = None,
    text = elementText
  )
}
