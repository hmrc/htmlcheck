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

import org.jsoup.nodes.Element

trait ElementAttribute {
  def value: String

  override def toString = value
}

case class ElementId(value: String) extends ElementAttribute

object ElementId {

  def apply(element: Element): Option[ElementId] =
    element.id() match {
      case "" => None
      case id => Some(ElementId(id))
    }
}

case object Selected extends ElementAttribute {

  val value = "selected"
  type Selected = Selected.type

  def apply(element: Element): Option[Selected] =
    element.hasAttr("selected") match {
      case false => None
      case true => element.attr("selected") match {
        case selected if selected == value => Some(Selected)
        case "" => Some(Selected)
        case _ => None
      }
    }

}


case class ElementValue(value: String) extends ElementAttribute

object ElementValue {

  def apply(element: Element): Option[ElementValue] =
    element.`val`() match {
      case "" => None
      case value => Some(ElementValue(value))
    }
}

case class ElementText(value: String) extends ElementAttribute

object ElementText {

  def apply(element: Element): Option[ElementText] =
    element.text() match {
      case "" => None
      case value => Some(ElementText(value))
    }
}

case class ElementClass(value: String) extends ElementAttribute

object ElementClass {

  def apply(element: Element): Option[ElementClass] = Some(ElementClass(element.className()))
}
