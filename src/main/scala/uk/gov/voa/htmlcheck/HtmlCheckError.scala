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

import uk.gov.voa.htmlcheck.elements.{ElementClass, ElementId}

sealed trait HtmlCheckError extends Throwable {
  protected def message: String

  override def getMessage: String = message
}

case class ElementWithIdNotFound(id: ElementId) extends HtmlCheckError {
  val message = s"Element with id=$id not found"
}

case class ElementSiblingNotFound(id: Option[ElementId]) extends HtmlCheckError {
  val message = s"Element with id=$id has no direct next sibling"
}

case class ElementWithIdOfWrongType(id: Option[ElementId], expectedType: String, actualType: String) extends HtmlCheckError {
  val message = s"Element with id=$id is of $actualType while $expectedType expected"
}

case class ElementOfTypeNotFound(tagType: String, maybeMessage: Option[String] = None) extends HtmlCheckError {
  val message = s"Element of type=$tagType not found${maybeMessage.map(message => s" $message").getOrElse("")}"
}

case class NoElementsOfClassFound(tagType: String, className: ElementClass) extends HtmlCheckError {
  val message = s"Elements of type=$tagType having class=$className not found"
}

case class ElementOutOfBounds(tagType: String, size: Int, index: Int) extends HtmlCheckError {
  val message = s"Element of type=$tagType with index $index not found in sequence of length $size"
}
