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
import uk.gov.voa.htmlcheck.{ElementOfWrongType, AttributeNotFound}
import uk.gov.voa.htmlcheck.elements.A.HrefAttribute
import uk.gov.voa.htmlcheck.tooling.UnitSpec

class ASpec extends UnitSpec {

  "elementWrapper" should {

    "successfully instantiate from a 'a' html tag" in {
      val snippet =
        """
          |<a href="url">text</a>
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      A.elementWrapper(element) shouldBe Right(A(element))
    }

    "return ElementWithIdOfWrongType when instantiated from a non 'a' html tag" in {
      val snippet =
        """
          |<div />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      A.elementWrapper(element) shouldBe Left(ElementOfWrongType("a", "div", Left(AttributeNotFound(AttributeName("id")))))
    }
  }

  "href" should {

    "return HrefAttribute if the href is defined on the 'a' tag" in {
      val snippet =
        """
          |<a href="url">text</a>
          |""".stripMargin

      val element = A(Jsoup.parse(snippet).body().children().first())

      element.href shouldBe Right(HrefAttribute("url"))
    }

    "return None for href when the attribute is not defined on the 'a' tag" in {
      val snippet =
        """
          |<a>text</a>
          |""".stripMargin

      val element = A(Jsoup.parse(snippet).body().children().first())

      element.href shouldBe Left(AttributeNotFound(AttributeName("href")))
    }

    "return None for href when the attribute has no value" in {
      val snippet =
        """
          |<a href="">text</a>
          |""".stripMargin

      val element = A(Jsoup.parse(snippet).body().children().first())

      element.href shouldBe Left(AttributeNotFound(AttributeName("href")))
    }
  }
}
