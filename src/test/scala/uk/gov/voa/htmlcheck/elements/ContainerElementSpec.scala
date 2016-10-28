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
import org.jsoup.nodes.Element
import uk.gov.voa.htmlcheck.Html.Implicits._
import uk.gov.voa.htmlcheck.elements.ElementAttribute._
import uk.gov.voa.htmlcheck.tooling.UnitSpec
import uk.gov.voa.htmlcheck.{NoElementsFound, _}

class ContainerElementSpec extends UnitSpec {

  "firstChild" should {

    "return HtmlCheckError when there are no children" in new TestCase {
      val Left(error) = parent.findOnlyDescendantBy[IdAttribute, Div](IdAttribute("inner-div")).flatMap(_.firstChild[Div])
      error shouldBe a[ElementOfTypeNotFound]
    }

    "return HtmlCheckError when the first child is of different type" in new TestCase {
      val Left(error) = parent.firstChild[Li]
      error shouldBe a[ElementOfWrongType]
    }

    "return the first child of the requested type" in new TestCase {
      parent.firstChild[TextArea].getOrError.id shouldBe Right(IdAttribute("1"))
    }
  }

  "onlyChild" should {

    val snippet =
      """<div id="div1" />
        |<div id="div2">
        | <p id="p"></p>
        |</div>
        |<div id="div3">
        | <p id="p1"></p>
        | <p id="p1"></p>
        |</div>
        |""".toHtml

    "return ElementOfTypeNotFound when there are no children" in new TestCase {

      val div = snippet.findOnlyDescendantBy[IdAttribute, Div](IdAttribute("div1")).getOrError

      div.onlyChild[Div] shouldBe Left(ElementOfTypeNotFound("div", Some("as the only child")))
    }

    "return ElementOfWrongType when the only child is of different type" in new TestCase {

      val div = snippet.findOnlyDescendantBy[IdAttribute, Div](IdAttribute("div2")).getOrError

      div.onlyChild[Div] shouldBe Left(ElementOfWrongType("div", "p", Right(IdAttribute("p"))))
    }

    "return HtmlCheckError when there is more than one child even of different type" in new TestCase {

      val div = snippet.findOnlyDescendantBy[IdAttribute, Div](IdAttribute("div3")).getOrError

      div.onlyChild[P] shouldBe Left(MoreThanOneElementFound(2, "p"))
    }

    "return the child of the requested type" in new TestCase {

      val div = snippet.findOnlyDescendantBy[IdAttribute, Div](IdAttribute("div2")).getOrError

      div.onlyChild[P].getOrError.id shouldBe Right(IdAttribute("p"))
    }
  }

  "findOnlyDescendantBy id" should {

    "return NoElementsFound when no descendant found with the given id" in new TestCase {
      parent.findOnlyDescendantBy[IdAttribute, Li]("invalid-id") shouldBe Left(NoElementsFound("li", IdAttribute("invalid-id")))
    }

    "return a descendant with the given id" in new TestCase {
      parent.findOnlyDescendantBy[IdAttribute, TextArea]("1").getOrError.id shouldBe Right(IdAttribute("1"))
    }
  }

  "findOnlyDescendantBy className" should {

    val div =
      """<div id="div1">
        | <div id="div2" class="div-class">
        |   <p id="p1" class="p-class"></p>
        | </div>
        | <div id="div3" class="div-class" />
        |</div>
        |""".toHtml.findOnlyDescendantBy[IdAttribute, Div]("div1").getOrError

    "return NoElementsFound when no descendant found with the given class name" in new TestCase {
      div.findOnlyDescendantBy[ClassAttribute, Div]("invalid-class") shouldBe Left(NoElementsFound("div", ClassAttribute("invalid-class")))
    }

    "return HtmlCheckError when more than one descendant found with the given class name" in new TestCase {
      div.findOnlyDescendantBy[ClassAttribute, Div](ClassAttribute("div-class")) shouldBe Left(MoreThanOneElementFound(2, "div", ClassAttribute("div-class")))
    }

    "return descendant with the given class name" in new TestCase {
      div.findOnlyDescendantBy[ClassAttribute, P](ClassAttribute("p-class")).getOrError.id.asString shouldBe "p1"
    }
  }

  "findFirstDescendantBy id" should {

    "return NoElementsFound when no descendant found with the given id" in new TestCase {
      parent.findFirstDescendantBy[IdAttribute, Li]("invalid-id") shouldBe Left(NoElementsFound("li", IdAttribute("invalid-id")))
    }

    "return the first descendant with the given id" in new TestCase {
      parent.findFirstDescendantBy[IdAttribute, TextArea]("1").getOrError.id shouldBe Right(IdAttribute("1"))
    }
  }

  "findFirstDescendantBy className" should {

    val div =
      """<div id="div1">
        | <div id="div2" class="div-class">
        |   <p id="p1" class="p-class"></p>
        | </div>
        | <div id="div3" class="div-class">
        |   <p id="p2" class="p-class"></p>
        | </div>
        |</div>
        |""".toHtml.findFirstDescendantBy[IdAttribute, Div]("div1").getOrError

    "return NoElementsFound when no descendant found with the given class name" in new TestCase {
      div.findFirstDescendantBy[ClassAttribute, Div]("invalid-class") shouldBe Left(NoElementsFound("div", ClassAttribute("invalid-class")))
    }

    "return the first descendant when there are more than one descendant with the given class name" in new TestCase {
      div.findFirstDescendantBy[ClassAttribute, P](ClassAttribute("p-class")).getOrError.id.asString shouldBe "p1"
    }

    "return descendant with the given class name if there's just one" in new TestCase {
      div.findFirstDescendantBy[ClassAttribute, P](ClassAttribute("p-class")).getOrError.id.asString shouldBe "p1"
    }
  }

  "findFirstChildOfType" should {

    "return ElementOfTypeNotFound when no children of the given type found" in new TestCase {
      parent.findFirstChildOfType[Li] shouldBe Left(NoElementsFound("li"))
    }

    "return first child of the given type when it exists" in new TestCase {
      val foundChildren = parent.findFirstChildOfType[TextArea]

      foundChildren.getOrError.id shouldBe Right(IdAttribute("1"))
    }
  }

  "findOnlyChildOfType" should {

    "return ElementOfTypeNotFound when no children of the given type found" in new TestCase {
      parent.findOnlyChildOfType[Li] shouldBe Left(NoElementsFound("li"))
    }

    "return MoreThanOneElementFound when more than one child of the given type found" in new TestCase {
      parent.findOnlyChildOfType[TextArea] shouldBe Left(MoreThanOneElementFound(2, "textarea"))
    }

    "return a child of the given type when just one exists" in new TestCase {
      parent.findOnlyChildOfType[Div].getOrError.id shouldBe Right(IdAttribute("inner-div"))
    }
  }

  "findChildrenOfType" should {

    "return ElementOfTypeNotFound when no children of the given type found" in new TestCase {
      parent.findChildrenOfType[Li] shouldBe Left(NoElementsFound("li"))
    }

    "return all children of the given type when they exist" in new TestCase {
      val foundChildren = parent.findChildrenOfType[TextArea]

      foundChildren.getOrError should have size 2

      foundChildren.getOrError.head.id shouldBe Right(IdAttribute("1"))
      foundChildren.getOrError(1).id shouldBe Right(IdAttribute("2"))
    }
  }

  "findChildOfTypeByIndex" should {

    "return child of the given type based on index in sequence" in new TestCase {
      parent.findChildOfTypeByIndex[TextArea](1).getOrError.id shouldBe Right(IdAttribute("2"))
    }

    "return ElementOutOfBounds error when no child found on that index" in new TestCase {
      parent.findChildOfTypeByIndex[TextArea](10) shouldBe Left(ElementOutOfBounds("textarea", 2, 10))
    }
  }

  "findChildrenBy id" should {

    val div =
      """<div id="div">
        | <textarea id="1" class="area-class1"></textArea>
        | <p id="p1"></p>
        | <textarea id="1" class="area-class2"></textArea>
        |</div>
        |""".toHtml.findOnlyDescendantBy[IdAttribute, Div](IdAttribute("div")).getOrError

    "return all children of the required type with the given id" in new TestCase {
      div.findChildrenBy[IdAttribute, TextArea](IdAttribute("1")).getOrError.map(_.className.asString) shouldBe Seq(
        "area-class1",
        "area-class2"
      )
    }

    "return no children if there are no children with the given id" in new TestCase {
      div.findChildrenBy[IdAttribute, TextArea](IdAttribute("unknown")) shouldBe Left(NoElementsFound("textarea", IdAttribute("unknown")))
    }

    "return no children if there are children with the given id but of different type" in new TestCase {
      div.findChildrenBy[IdAttribute, TextArea](IdAttribute("p1")) shouldBe Left(NoElementsFound("textarea", IdAttribute("p1")))
    }
  }

  "findChildrenBy className" should {

    val div =
      """<div id="div">
        | <textarea id="1" class="area-class"></textArea>
        | <p></p>
        | <textarea id="2" class="area-class"></textArea>
        |</div>
        |""".toHtml.findOnlyDescendantBy[IdAttribute, Div](IdAttribute("div")).getOrError

    "return all children of the required type with the given class" in new TestCase {
      div.findChildrenBy[ClassAttribute, TextArea](ClassAttribute("area-class")).getOrError.map(_.id.asString) shouldBe Seq(
        "1",
        "2"
      )
    }

    "return no children if there are no children with the given class" in new TestCase {
      div.findChildrenBy[ClassAttribute, TextArea](ClassAttribute("unknown-class")) shouldBe Left(NoElementsFound("textarea", ClassAttribute("unknown-class")))
    }

    "return no children if there are children with the given class but of different type" in new TestCase {
      div.findChildrenBy[ClassAttribute, P](ClassAttribute("area-class")) shouldBe Left(NoElementsFound("p", ClassAttribute("area-class")))
    }
  }

  "findFirstChildBy id" should {

    "return first child of the required type having the given id" in new TestCase {
      parent.findFirstChildBy[IdAttribute, TextArea](IdAttribute("1")).getOrError.id shouldBe Right(IdAttribute("1"))
    }

    "return error if there are no children of the given id" in new TestCase {
      parent.findFirstChildBy[IdAttribute, TextArea](IdAttribute("unknown")) shouldBe Left(NoElementsFound("textarea", IdAttribute("unknown")))
    }

    "return error if there are children with the given id but of different type" in new TestCase {
      parent.findFirstChildBy[IdAttribute, P](IdAttribute("inner-div")) shouldBe Left(NoElementsFound("p", IdAttribute("inner-div")))
    }
  }

  "findFirstChildBy elementClass" should {

    "return first child of the required type having the given class" in new TestCase {
      parent.findFirstChildBy[ClassAttribute, TextArea](ClassAttribute("area-class")).getOrError.id shouldBe Right(IdAttribute("1"))
    }

    "return error if there are no children of the given class" in new TestCase {
      parent.findFirstChildBy[ClassAttribute, TextArea](ClassAttribute("unknown-class")) shouldBe Left(NoElementsFound("textarea", ClassAttribute("unknown-class")))
    }

    "return error if there are children of the given class but of different type" in new TestCase {
      parent.findFirstChildBy[ClassAttribute, P](ClassAttribute("area-class")) shouldBe Left(NoElementsFound("p", ClassAttribute("area-class")))
    }
  }

  "findOnlyChildBy elementClass" should {

    val div =
      """<div id="div">
        | <textarea id="1" class="area-class"></textArea>
        | <p id="p1" class="p-class"></p>
        | <textarea id="2" class="area-class"></textArea>
        |</div>
        |""".toHtml.findOnlyDescendantBy[IdAttribute, Div](IdAttribute("div")).getOrError

    "return found child of the required type having the given class" in new TestCase {
      div.findOnlyChildBy[ClassAttribute, P](ClassAttribute("p-class")).getOrError.id shouldBe Right(IdAttribute("p1"))
    }

    "return no children if there are more than one of the given class" in new TestCase {
      div.findOnlyChildBy[ClassAttribute, TextArea](ClassAttribute("area-class")) shouldBe Left(MoreThanOneElementFound(2, "textarea", ClassAttribute("area-class")))
    }

    "return no children if there are no children of the given class" in new TestCase {
      div.findOnlyChildBy[ClassAttribute, TextArea](ClassAttribute("unknown-class")) shouldBe Left(NoElementsFound("textarea", ClassAttribute("unknown-class")))
    }

    "return no children if there are children of the given class but of different type" in new TestCase {
      div.findOnlyChildBy[ClassAttribute, P](ClassAttribute("area-class")) shouldBe Left(NoElementsFound("p", ClassAttribute("area-class")))
    }
  }

  "findOnlyChildBy id" should {

    val div =
      """<div id="div">
        | <textarea id="1"></textArea>
        | <p id="p1"></p>
        | <textarea id="1"></textArea>
        |</div>
        |""".toHtml.findOnlyDescendantBy[IdAttribute, Div](IdAttribute("div")).getOrError

    "return found child of the required type having the given id" in new TestCase {
      div.findOnlyChildBy[IdAttribute, P](IdAttribute("p1")).getOrError.id shouldBe Right(IdAttribute("p1"))
    }

    "return no children if there are more than one of the given id" in new TestCase {
      div.findOnlyChildBy[IdAttribute, TextArea](IdAttribute("1")) shouldBe Left(MoreThanOneElementFound(2, "textarea", IdAttribute("1")))
    }

    "return no children if there are no children of the given id" in new TestCase {
      div.findOnlyChildBy[IdAttribute, TextArea](IdAttribute("unknown")) shouldBe Left(NoElementsFound("textarea", IdAttribute("unknown")))
    }

    "return no children if there are children of the given id but of different type" in new TestCase {
      div.findOnlyChildBy[IdAttribute, P](IdAttribute("1")) shouldBe Left(NoElementsFound("p", IdAttribute("1")))
    }
  }

  private trait TestCase {
    self =>

    val parent = new ContainerElement with HtmlElement with ElementProperties {
      val element: Element =
        """
          |<div id="div">
          | <textarea id="1" class="area-class"></textArea>
          | <p></p>
          | <textarea id="2" class="area-class"></textArea>
          | <div id="inner-div"></div>
          |</div>
        """.parse.getElementById("div")
    }
  }

}
