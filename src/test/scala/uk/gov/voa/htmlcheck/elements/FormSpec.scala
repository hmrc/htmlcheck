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
import uk.gov.voa.htmlcheck.ElementOfWrongType
import uk.gov.voa.htmlcheck.elements.Form.MethodAttribute.{Get, Post}
import uk.gov.voa.htmlcheck.elements.Form._
import uk.gov.voa.htmlcheck.tooling.UnitSpec

class FormSpec extends UnitSpec {

  "elementWrapper" should {

    "successfully instantiate from a 'form' html tag" in {
      val snippet =
        """
          |<form />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Form.elementWrapper(element) shouldBe Right(Form(element))
    }

    "return ElementWithIdOfWrongType when instantiated from a non 'form' html tag" in {
      val snippet =
        """
          |<div />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Form.elementWrapper(element) shouldBe Left(ElementOfWrongType("form", "div", None))
    }
  }

  "action" should {

    "return ActionAttribute if the 'action' is defined on the tag" in {
      val snippet =
        """
          |<form action="url" />
          |""".stripMargin

      val element = Form(Jsoup.parse(snippet).body().children().first())

      element.action shouldBe Some(ActionAttribute("url"))
    }

    "return None for 'action' when the attribute is not defined on the tag" in {
      val snippet =
        """
          |<form />
          |""".stripMargin

      val element = Form(Jsoup.parse(snippet).body().children().first())

      element.action shouldBe None
    }

    "return None for 'for' when the attribute has no value" in {
      val snippet =
        """
          |<form action="" />
          |""".stripMargin

      val element = Form(Jsoup.parse(snippet).body().children().first())

      element.action shouldBe None
    }
  }

  "method" should {

    "return Get if the 'method' is Not defined" in {
      val snippet =
        """
          |<form />
          |""".stripMargin

      val element = Form(Jsoup.parse(snippet).body().children().first())

      element.method shouldBe Get
    }

    "return Get if the 'method' is defined as Get the tag" in {
      val snippet =
        """
          |<form method="get" />
          |""".stripMargin

      val element = Form(Jsoup.parse(snippet).body().children().first())

      element.method shouldBe Get
    }

    "return Post if the 'method' is defined as Post the tag" in {
      val snippet =
        """
          |<form method="post" />
          |""".stripMargin

      val element = Form(Jsoup.parse(snippet).body().children().first())

      element.method shouldBe Post
    }
  }

}
