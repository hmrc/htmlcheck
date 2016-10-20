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

case class Select(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement {

  lazy val options: Xor[HtmlCheckError, Seq[SelectOption]] = findChildrenOfType[SelectOption]

  lazy val selectedOptions: Xor[HtmlCheckError, Seq[SelectOption]] = options.map(_.filter(_.selected))

}

object Select {

  implicit def selectElementWrapper(element: Element): HtmlCheckError Xor Select =
    if (element.tagName() != "select")
      Left(ElementWithIdOfWrongType(ElementId(element), "select", element.tagName()))
    else
      Right(Select(element))

}

case class SelectOption(protected val element: Element)
  extends HtmlElement
    with ElementProperties
    with ContainerElement {

  lazy val selected: Boolean = element.attr("selected") == "selected"
}

object SelectOption {

  implicit def optionElementWrapper(element: Element): HtmlCheckError Xor SelectOption =
    if (element.tagName() != "option")
      Left(ElementWithIdOfWrongType(ElementId(element), "option", element.tagName()))
    else
      Right(SelectOption(element))
}
