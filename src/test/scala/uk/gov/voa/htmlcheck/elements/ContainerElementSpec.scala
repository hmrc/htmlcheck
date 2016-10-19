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
import uk.gov.voa.htmlcheck.{ElementOfTypeNotFound, ElementOutOfBounds, ElementWithIdOfWrongType, HtmlCheckError}

class ContainerElementSpec extends UnitSpec {

  "firstChild" should {

    "return HtmlCheckError when there are no children" in new TestCase {
      val Left(error) = parent.findChildById[Div]("inner-div").flatMap(_.firstChild[Div])
      error shouldBe a[ElementOfTypeNotFound]
    }

    "return HtmlCheckError when the first child is of different type" in new TestCase {
      val Left(error) = parent.firstChild[Li]
      error shouldBe a[ElementWithIdOfWrongType]
    }

    "return the first child of the requested type" in new TestCase {
      parent.firstChild[TextArea].getOrError.elementId shouldBe Some(ElementId("1"))
    }
  }

  "findChildById" should {

    "return HtmlCheckError when no child found with the given id" in new TestCase {
      val Left(error) = parent.findChildById[Li]("invalid-id")
      error shouldBe a[HtmlCheckError]
    }

    "return child with the given id" in new TestCase {
      parent.findChildById[TextArea]("1").getOrError.elementId shouldBe Some(ElementId("1"))
    }
  }

  "findFirstChildOfType" should {

    "return ElementOfTypeNotFound when no children of the given type found" in new TestCase {
      parent.findFirstChildOfType[Li] shouldBe Left(ElementOfTypeNotFound("li"))
    }

    "return first child of the given type when it exists" in new TestCase {
      val foundChildren = parent.findFirstChildOfType[TextArea]

      foundChildren.getOrError.elementId shouldBe Some(ElementId("1"))
    }
  }

  "findChildrenOfType" should {

    "return ElementOfTypeNotFound when no children of the given type found" in new TestCase {
      parent.findChildrenOfType[Li] shouldBe Left(ElementOfTypeNotFound("li"))
    }

    "return all children of the given type when they exist" in new TestCase {
      val foundChildren = parent.findChildrenOfType[TextArea]

      foundChildren.getOrError should have size 2

      foundChildren.getOrError.head.elementId shouldBe Some(ElementId("1"))
      foundChildren.getOrError(1).elementId shouldBe Some(ElementId("2"))
    }
  }

  "findChildOfTypeByIndex" should {

    "return child of the given type based on index in sequence" in new TestCase {
      parent.findChildOfTypeByIndex[TextArea](1).getOrError.elementId shouldBe Some(ElementId("2"))
    }

    "return ElementOutOfBounds error when no child found on that index" in new TestCase {
      parent.findChildOfTypeByIndex[TextArea](10) shouldBe Left(ElementOutOfBounds("textarea", 2, 10))
    }
  }

  private trait TestCase {
    self =>

    val element = Jsoup.parse(
      """
        |<div id="div">
        | <textarea id="1"></textArea>
        | <p></p>
        | <textarea id="2"></textArea>
        | <div id="inner-div"></div>
        |</div>
      """.stripMargin).getElementById("div")

    val parent = new ContainerElement with HtmlElement with ElementProperties {
      val element: Element = self.element
    }
  }

}
