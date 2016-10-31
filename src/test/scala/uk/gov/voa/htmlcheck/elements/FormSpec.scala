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

import cats.data.Xor
import cats.data.Xor.{Left, Right}
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import uk.gov.voa.htmlcheck.elements.Form.MethodAttribute.{Get, Post}
import uk.gov.voa.htmlcheck.elements.Form._
import uk.gov.voa.htmlcheck.{AttributeNotFound, HtmlCheckError}

class FormSpec extends HtmlElementSpec[Form] {

  lazy val tagName = "form"

  lazy val elementWrapper: (Element) => HtmlCheckError Xor Form = Form.elementWrapper

  lazy val elementApply: (Element) => Form = Form.apply

  "action" should {

    "return ActionAttribute if the 'action' is defined on the tag" in {
      val snippet =
        """
          |<form action="url" />
          |""".stripMargin

      val element = Form(Jsoup.parse(snippet).body().children().first())

      element.action shouldBe Right(ActionAttribute("url"))
    }

    "return None for 'action' when the attribute is not defined on the tag" in {
      val snippet =
        """
          |<form />
          |""".stripMargin

      val element = Form(Jsoup.parse(snippet).body().children().first())

      element.action shouldBe Left(AttributeNotFound(AttributeName("action")))
    }

    "return None for 'for' when the attribute has no value" in {
      val snippet =
        """
          |<form action="" />
          |""".stripMargin

      val element = Form(Jsoup.parse(snippet).body().children().first())

      element.action shouldBe Left(AttributeNotFound(AttributeName("action")))
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
