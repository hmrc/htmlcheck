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
import uk.gov.voa.htmlcheck.Html.Implicits._
import uk.gov.voa.htmlcheck.elements.ElementAttribute.TypeAttribute
import uk.gov.voa.htmlcheck.tooling.UnitSpec
import uk.gov.voa.htmlcheck.{AttributeNotFound, ElementOfWrongType}

class ButtonSpec extends UnitSpec {

  "elementWrapper" should {

    "successfully instantiate from a 'button' of type button html tag" in {
      val snippet =
        """
          |<button type="button">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Button.elementWrapper(element) shouldBe Right(GenericButton(element))
    }

    "successfully instantiate from a 'button' of type submit html tag" in {
      val snippet =
        """
          |<button type="submit">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Button.elementWrapper(element) shouldBe Right(ButtonSubmit(element))
    }

    "successfully instantiate from a 'button' of type reset html tag" in {
      val snippet =
        """
          |<button type="reset">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Button.elementWrapper(element) shouldBe Right(ButtonReset(element))
    }

    "return ElementWithIdOfWrongType when instantiated from a non 'input' html tag" in {
      val snippet =
        """
          |<div />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Button.elementWrapper(element) shouldBe Left(ElementOfWrongType("button", "div", Left(AttributeNotFound(AttributeName("id")))))
    }
  }

  "typeAttribute" should {

    "return button's type if it is defined" in {

      val snippet =
        """
          |<button type="reset">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Button.elementWrapper(element).getOrError.typeAttribute shouldBe Right(TypeAttribute("reset"))
    }

    "return no button's type if it is not defined" in {

      val snippet =
        """
          |<button>
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Button.elementWrapper(element).getOrError.typeAttribute shouldBe a[Left[_]]
    }
  }
}

class ButtonSubmitSpec extends UnitSpec {

  "elementWrapper" should {

    "successfully instantiate from a 'button' of type submit html tag" in {
      val snippet =
        """
          |<button type="submit">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      ButtonSubmit.elementWrapper(element) shouldBe Right(ButtonSubmit(element))
    }

    "return ElementWithIdOfWrongType when instantiated from a non 'input' html tag" in {
      val snippet =
        """
          |<div />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      ButtonSubmit.elementWrapper(element) shouldBe Left(ElementOfWrongType("button-submit", "div", Left(AttributeNotFound(AttributeName("id")))))
    }

    "return ElementWithIdOfWrongType when instantiated from an 'button' of non submit html tag" in {
      val snippet =
        """
          |<button type="text">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      ButtonSubmit.elementWrapper(element) shouldBe Left(ElementOfWrongType("button-submit", "button-text", Left(AttributeNotFound(AttributeName("id")))))
    }
  }

}

class ButtonResetSpec extends UnitSpec {

  "elementWrapper" should {

    "successfully instantiate from a 'button' of type reset html tag" in {
      val snippet =
        """
          |<button type="reset">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      ButtonReset.elementWrapper(element) shouldBe Right(ButtonReset(element))
    }

    "return ElementWithIdOfWrongType when instantiated from a non 'input' html tag" in {
      val snippet =
        """
          |<div />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      ButtonReset.elementWrapper(element) shouldBe Left(ElementOfWrongType("button-reset", "div", Left(AttributeNotFound(AttributeName("id")))))
    }

    "return ElementWithIdOfWrongType when instantiated from an 'button' of non reset html tag" in {
      val snippet =
        """
          |<button type="text">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      ButtonReset.elementWrapper(element) shouldBe Left(ElementOfWrongType("button-reset", "button-text", Left(AttributeNotFound(AttributeName("id")))))
    }
  }

}
