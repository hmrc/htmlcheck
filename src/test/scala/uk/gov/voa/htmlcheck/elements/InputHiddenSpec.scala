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

class InputHiddenSpec extends UnitSpec {

  "elementWrapper" should {

    "successfully instantiate from an 'input' of type hidden html tag" in {
      val snippet =
        """
          |<input type="hidden">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      InputHidden.elementWrapper(element) shouldBe Right(InputHidden(element))
    }

    "return ElementWithIdOfWrongType when instantiated from a non 'input' html tag" in {
      val snippet =
        """
          |<div />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      InputHidden.elementWrapper(element) shouldBe Left(ElementOfWrongType("input-hidden", "div", Left(AttributeNotFound(AttributeName("id")))))
    }

    "return ElementWithIdOfWrongType when instantiated from an 'input' of non text html tag" in {
      val snippet =
        """
          |<input type="radio">
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      InputHidden.elementWrapper(element) shouldBe Left(ElementOfWrongType("input-hidden", "input-radio", Left(AttributeNotFound(AttributeName("id")))))
    }
  }

}
