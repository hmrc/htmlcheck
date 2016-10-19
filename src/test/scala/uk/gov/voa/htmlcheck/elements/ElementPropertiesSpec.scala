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

import cats.data.Xor._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import uk.gov.voa.htmlcheck.Html._
import uk.gov.voa.htmlcheck.tooling.UnitSpec
import uk.gov.voa.htmlcheck.{ElementSiblingNotFound, ElementWithIdOfWrongType}

class ElementPropertiesSpec extends UnitSpec {

  "id" should {

    "return element's id if it has one" in new TestCase {
      parent.id shouldBe Some(ElementId("div1"))
    }

    "return None if there's no id defined for the element" in {
      val element = Div(Jsoup.parse("<div />").getAllElements.first())

      element.id shouldBe None
    }
  }

  "nextSibling" should {

    "return ElementWithIdOfWrongType when there's no direct child sibling of the given type" in new TestCase {
      val Left(error) = parent.findChildById[TextArea](ElementId("1"))
        .flatMap(_.nextSibling[TextArea])

      error shouldBe ElementWithIdOfWrongType(Some(ElementId("p1")), "textarea", "p")
    }

    "return ElementSiblingNotFound when an element is the last child" in new TestCase {
      val Left(error) = parent.findChildById[TextArea](ElementId("2"))
        .flatMap(_.nextSibling[TextArea])

      error shouldBe ElementSiblingNotFound(Some(ElementId("2")))
    }

    "return child direct with the given id" in new TestCase {
      parent.findChildById[TextArea](ElementId("1"))
        .flatMap(_.nextSibling[P])
        .getOrError.elementId shouldBe Some(ElementId("p1"))
    }
  }

  private trait TestCase {
    self =>

    val element = Jsoup.parse(
      """
        |<div id="div1">
        | <textarea id="1"></textArea>
        | <p id="p1"></p>
        | <textarea id="2"></textArea>
        |</div>
      """.stripMargin).getElementById("div1")

    val parent = new HtmlElement with ElementProperties with ContainerElement {
      val element: Element = self.element
    }
  }

}
