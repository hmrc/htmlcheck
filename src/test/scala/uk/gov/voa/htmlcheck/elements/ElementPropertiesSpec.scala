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
import uk.gov.voa.htmlcheck.Html.Implicits._
import uk.gov.voa.htmlcheck.elements.ElementAttribute._
import uk.gov.voa.htmlcheck.tooling.UnitSpec
import uk.gov.voa.htmlcheck.{AttributeNotFound, ElementOfWrongType, ElementSiblingNotFound, NoElementsFound}

class ElementPropertiesSpec extends UnitSpec {

  "id" should {

    "return element's id if it has one" in new TestCase {
      parent.id shouldBe Right(IdAttribute("div1"))
    }

    "return None if there's no id defined for the element" in {
      val element = Legend(Jsoup.parse("<div />").getAllElements.first())

      element.id shouldBe Left(AttributeNotFound(AttributeName("id")))
    }
  }

  "nextSibling" should {

    "return ElementWithIdOfWrongType when there's no direct child sibling of the given type" in new TestCase {
      val Left(error) = parent.findOnlyDescendantBy[IdAttribute, TextArea](IdAttribute("1"))
        .flatMap(_.nextSibling[TextArea])

      error shouldBe ElementOfWrongType("textarea", "p", Right(IdAttribute("p1")))
    }

    "return ElementSiblingNotFound when an element is the last child" in new TestCase {
      val Left(error) = parent.findOnlyDescendantBy[IdAttribute, TextArea](IdAttribute("2"))
        .flatMap(_.nextSibling[TextArea])

      error shouldBe ElementSiblingNotFound("has no direct next sibling", Right(IdAttribute("2")))
    }

    "return child direct with the given id" in new TestCase {
      parent.findOnlyDescendantBy[IdAttribute, TextArea](IdAttribute("1"))
        .flatMap(_.nextSibling[P])
        .getOrError.id shouldBe Right(IdAttribute("p1"))
    }
  }

  "findNextClosestSiblingOfType" should {

    val snippet =
      """
        |<div id="div">
        | <textarea id="1"></textArea>
        | <p id="p1"></p>
        | <div id="inner-div">
        |   <p />
        | </div>
        | <textarea id="2"></textArea>
        | <textarea id="3"></textArea>
        |</div>
      """.toHtml

    "return ElementSiblingNotFound when an element is the last child" in new TestCase {

      val textArea = snippet.findFirstDescendantBy[IdAttribute, TextArea]("3").getOrError

      textArea.findNextClosestSiblingOfType[Li] shouldBe Left(ElementSiblingNotFound("has no next sibling of type 'li'", Right(IdAttribute("3"))))
    }

    "return ElementSiblingNotFound when there is no following sibling of the required type" in new TestCase {

      val p = snippet.findFirstDescendantBy[IdAttribute, P]("p1").getOrError

      p.findNextClosestSiblingOfType[P] shouldBe Left(ElementSiblingNotFound("has no next sibling of type 'p'", Right(IdAttribute("p1"))))
    }

    "return the first following sibling of the given type" in {

      val textArea = snippet.findFirstDescendantBy[IdAttribute, TextArea]("1").getOrError

      textArea.findNextClosestSiblingOfType[TextArea].getOrError.id shouldBe Right(IdAttribute("2"))
    }
  }

  private trait TestCase {

    val parent = new HtmlElement with ElementProperties with ContainerElement {
      val element: Element =
        """
          |<div id="div1">
          | <textarea id="1"></textArea>
          | <p id="p1"></p>
          | <textarea id="2"></textArea>
          |</div>
        """.parse.getElementById("div1")
    }
  }

}
