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

case class AttributeName(value: String) {
  require(value.nonEmpty)

  override def toString = value
}

trait ElementAttribute {
  require(value.nonEmpty)

  def value: String

  override def toString = value
}

object ElementAttribute {

  case class GenericAttribute(name: AttributeName, value: String) extends ElementAttribute {
    require(value.nonEmpty)

    override def toString = value
  }

  object GenericAttribute {

    def apply(name: AttributeName, element: Element): Option[GenericAttribute] =
      element.hasAttr(name.value) match {
        case false => None
        case true => element.attr(name.value) match {
          case "" => None
          case value => Some(GenericAttribute(name, value))
        }
      }
  }

  case class IdAttribute(value: String) extends ElementAttribute

  object IdAttribute {

    def apply(element: Element): Option[IdAttribute] =
      element.id() match {
        case "" => None
        case id => Some(IdAttribute(id))
      }
  }

  case class NameAttribute(value: String) extends ElementAttribute

  object NameAttribute {

    def apply(element: Element): Option[NameAttribute] =
      element.attr("name") match {
        case "" => None
        case name => Some(NameAttribute(name))
      }
  }

  case object SelectedAttribute extends ElementAttribute {

    lazy val value = "selected"
    type SelectedAttribute = SelectedAttribute.type

    def apply(element: Element): Option[SelectedAttribute] =
      element.hasAttr("selected") match {
        case false => None
        case true => element.attr("selected") match {
          case selected if selected == value => Some(SelectedAttribute)
          case "" => Some(SelectedAttribute)
          case _ => None
        }
      }
  }

  case class ValueAttribute(value: String) extends ElementAttribute

  object ValueAttribute {

    def apply(element: Element): Option[ValueAttribute] =
      element.`val`() match {
        case "" => None
        case value => Some(ValueAttribute(value))
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

  case class ClassAttribute(value: String) extends ElementAttribute

  object ClassAttribute {

    def apply(element: Element): Option[ClassAttribute] = Some(ClassAttribute(element.className()))
  }

}
