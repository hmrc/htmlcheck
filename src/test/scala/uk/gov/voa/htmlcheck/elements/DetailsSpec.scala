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

import cats.data.Xor.{Left, Right}
import org.jsoup.Jsoup
import uk.gov.voa.htmlcheck.tooling.UnitSpec
import uk.gov.voa.htmlcheck.{AttributeNotFound, ElementOfWrongType}

class DetailsSpec extends UnitSpec {

  "elementWrapper" should {

    "successfully instantiate from a 'details' html tag" in {
      val snippet =
        """
          |<details />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Details.elementWrapper(element) shouldBe Right(Details(element))
    }

    "return ElementWithIdOfWrongType when instantiated from a non 'details' html tag" in {
      val snippet =
        """
          |<div />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Details.elementWrapper(element) shouldBe Left(ElementOfWrongType("details", "div", Left(AttributeNotFound(AttributeName("id")))))
    }
  }
}

class SummarySpec extends UnitSpec {

  "elementWrapper" should {

    "successfully instantiate from a 'summary' html tag" in {
      val snippet =
        """
          |<summary />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Summary.elementWrapper(element) shouldBe Right(Summary(element))
    }

    "return ElementWithIdOfWrongType when instantiated from a non 'summary' html tag" in {
      val snippet =
        """
          |<div />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Summary.elementWrapper(element) shouldBe Left(ElementOfWrongType("summary", "div", Left(AttributeNotFound(AttributeName("id")))))
    }
  }
}
