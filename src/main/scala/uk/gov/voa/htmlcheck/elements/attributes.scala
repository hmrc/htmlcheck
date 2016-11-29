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
import uk.gov.voa.htmlcheck.{AttributeNotFound, HtmlCheckError}

import scala.language.implicitConversions

case class AttributeName(value: String) {
  require(value.nonEmpty)

  override def toString = value
}

trait ElementAttribute

trait ElementAttributeWithValue extends ElementAttribute {

  def value: String

  override def toString = value
}

trait ElementAttributeWithOptionalValue extends ElementAttribute {

  def value: Option[String]

  override def toString = value.getOrElse("")
}

object ElementAttribute {

  object Implicits extends Implicits

  trait Implicits {

    implicit class XorElementAttributeOps(maybeAttribute: HtmlCheckError Xor ElementAttribute) {

      lazy val asString: String = maybeAttribute.fold(
        _ => "",
        attribute => attribute.toString
      )

      lazy val present: Boolean = maybeAttribute.isRight
    }

    implicit def stringToClassAttributeWrapper(className: String): ClassAttribute = ClassAttribute(className)

    implicit def stringToIdAttributeWrapper(id: String): IdAttribute = IdAttribute(id)

  }

  case class CustomAttribute(name: AttributeName, value: Option[String]) extends ElementAttributeWithOptionalValue

  object CustomAttribute {

    def apply(name: AttributeName, element: Element): HtmlCheckError Xor CustomAttribute =
      element.hasAttr(name.value) match {
        case false => Left(AttributeNotFound(name))
        case true => element.attr(name.value) match {
          case "" => Right(CustomAttribute(name, value = None))
          case value => Right(CustomAttribute(name, Some(value)))
        }
      }
  }

  case class IdAttribute(value: String) extends ElementAttributeWithValue

  object IdAttribute {

    def apply(element: Element): HtmlCheckError Xor IdAttribute =
      element.id() match {
        case "" => Left(AttributeNotFound(AttributeName("id")))
        case id => Right(IdAttribute(id))
      }
  }

  case class TypeAttribute(value: String) extends ElementAttributeWithValue

  object TypeAttribute {

    def apply(element: Element): HtmlCheckError Xor TypeAttribute =
      element.attr("type") match {
        case "" => Left(AttributeNotFound(AttributeName("type")))
        case t => Right(TypeAttribute(t))
      }
  }

  case class TagAttribute(value: String) extends ElementAttributeWithValue

  object TagAttribute {

    def apply(element: Element): HtmlCheckError Xor TagAttribute =
      element.tagName() match {
        case "" => Left(AttributeNotFound(AttributeName("tag")))
        case tag => Right(TagAttribute(tag))
      }
  }

  case class NameAttribute(value: String) extends ElementAttributeWithValue

  object NameAttribute {

    def apply(element: Element): HtmlCheckError Xor NameAttribute =
      element.attr("name") match {
        case "" => Left(AttributeNotFound(AttributeName("name")))
        case name => Right(NameAttribute(name))
      }
  }

  case class ValueAttribute(value: String) extends ElementAttributeWithValue

  object ValueAttribute {

    def apply(element: Element): HtmlCheckError Xor ValueAttribute =
      element.`val`() match {
        case "" => Left(AttributeNotFound(AttributeName("value")))
        case value => Right(ValueAttribute(value))
      }
  }

  case class ClassAttribute(value: String) extends ElementAttributeWithValue

  object ClassAttribute {

    def apply(element: Element): HtmlCheckError Xor ClassAttribute =
      Right(ClassAttribute(element.className()))
  }

}
