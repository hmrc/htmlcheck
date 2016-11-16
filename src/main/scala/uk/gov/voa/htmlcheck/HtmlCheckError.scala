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

package uk.gov.voa.htmlcheck

import cats.data.Xor
import uk.gov.voa.htmlcheck.elements.{AttributeName, ElementAttribute}

sealed trait HtmlCheckError extends Throwable {
  protected def message: String

  override def getMessage: String = message
}

case class AttributeNotFound(attributeName: AttributeName) extends HtmlCheckError {
  val message = s"No '$attributeName' attribute found"
}

case class ElementSiblingNotFound(explanation: String,
                                  tagType: String,
                                  maybeAttribute: HtmlCheckError Xor ElementAttribute) extends HtmlCheckError {
  val message = s"Element of type '$tagType'" +
    s"${maybeAttribute.map(attribute => s" with ${attribute.getClass.getSimpleName}=$attribute").getOrElse("")}" +
    s" $explanation"
}

case class ElementOfWrongType(expectedType: String, actualType: String, maybeAttribute: HtmlCheckError Xor ElementAttribute) extends HtmlCheckError {
  val message = s"Found '$actualType' element while '$expectedType' expected${maybeAttribute.map(attribute => s" ${attribute.getClass.getSimpleName}=$attribute").getOrElse("")}"
}

case class ElementOfTypeNotFound(tagType: String, maybeMessage: Option[String] = None) extends HtmlCheckError {
  val message = s"'$tagType' element not found${maybeMessage.map(message => s" $message").getOrElse("")}"
}

case class NoElementsFound(tagType: String, maybeAttribute: Option[ElementAttribute]) extends HtmlCheckError {
  val message = s"Elements of type=$tagType${maybeAttribute.map(attribute => s" having ${attribute.getClass.getSimpleName}=$attribute").getOrElse("")} not found"
}

object NoElementsFound {

  def apply(tagType: String, attribute: ElementAttribute): NoElementsFound =
    NoElementsFound(tagType, Some(attribute))

  def apply(tagType: String): NoElementsFound =
    NoElementsFound(tagType, None)
}

case class MoreThanOneElementFound(numberOfFoundElements: Int, tagType: String, maybeAttribute: Option[ElementAttribute]) extends HtmlCheckError {
  val message = s"There are $numberOfFoundElements elements found but only one '$tagType'${maybeAttribute.map(attribute => s" having ${attribute.getClass.getSimpleName}=$attribute").getOrElse("")} was expected"
}

object MoreThanOneElementFound {

  def apply(numberOfFoundElements: Int, tagType: String, attribute: ElementAttribute): MoreThanOneElementFound =
    MoreThanOneElementFound(numberOfFoundElements, tagType, Some(attribute))

  def apply(numberOfFoundElements: Int, tagType: String): MoreThanOneElementFound =
    MoreThanOneElementFound(numberOfFoundElements, tagType, None)
}

case class ElementOutOfBounds(tagType: String, size: Int, index: Int) extends HtmlCheckError {
  val message = s"Element of type=$tagType with index $index not found in sequence of length $size"
}
