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
import uk.gov.voa.htmlcheck.tooling.UnitSpec
import uk.gov.voa.htmlcheck.{AttributeNotFound, ElementOfWrongType}

class InputCheckboxSpec extends UnitSpec {

  "elementWrapper" should {

    "successfully instantiate from an 'input' of type checkbox html tag" in {
      val snippet =
        """
          |<input type="checkbox">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      InputCheckbox.elementWrapper(element) shouldBe Right(InputCheckbox(element))
    }

    "return ElementWithIdOfWrongType when instantiated from a non 'input' html tag" in {
      val snippet =
        """
          |<div />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      InputCheckbox.elementWrapper(element) shouldBe Left(ElementOfWrongType("input-checkbox", "div", Left(AttributeNotFound(AttributeName("id")))))
    }

    "return ElementWithIdOfWrongType when instantiated from an 'input' of non checkbox html tag" in {
      val snippet =
        """
          |<input type="text">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      InputCheckbox.elementWrapper(element) shouldBe Left(ElementOfWrongType("input-checkbox", "input-text", Left(AttributeNotFound(AttributeName("id")))))
    }
  }

  "checked" should {

    "return true if there is a 'checked' property with value of checked" in {
      val snippet =
        """
          |<input type="checkbox" checked="checked">
          |""".stripMargin

      InputCheckbox(Jsoup.parse(snippet).body().children().first()).checked shouldBe true
    }

    "return true if there is just a 'checked' property without value" in {
      val snippet =
        """
          |<input type="checkbox" checked>
          |""".stripMargin

      InputCheckbox(Jsoup.parse(snippet).body().children().first()).checked shouldBe true
    }

    "return false if there is no 'checked' property" in {
      val snippet =
        """
          |<input type="checkbox">
          |""".stripMargin

      InputCheckbox(Jsoup.parse(snippet).body().children().first()).checked shouldBe false
    }
  }
}
