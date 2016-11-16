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

package uk.gov.voa.htmlcheck

import cats.data.Xor._
import uk.gov.voa.htmlcheck.tooling.UnitSpec

class HtmlSpec extends UnitSpec {

  val snippet =
    """
      |<html>
      | <head>
      |   <title>Page title</title>
      | </head>
      | <body>
      |   <p>abc</p>
      |   <div><p>def</p></div>
      | </body>
      |</html>
      |""".stripMargin

  "title" should {

    "return page head's title value" in new TestCase {
      html.title shouldBe Right("Page title")
    }

    "return an empty String if there's no title tag in the page's head" in {
      Html("<html />").title shouldBe Left(ElementOfTypeNotFound("title"))
    }
  }

  "text" should {

    "return textual representation of the html without any tags" in new TestCase {
      html.text shouldBe "Page title abc def"
    }

  }

  private trait TestCase {
    val html = Html(snippet)
  }
}
