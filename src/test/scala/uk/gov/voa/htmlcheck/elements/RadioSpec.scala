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

import org.jsoup.Jsoup
import uk.gov.voa.htmlcheck.tooling.UnitSpec

class RadioSpec extends UnitSpec {

  "checked" should {

    "return true if there is a 'checked' property with value of checked" in {
      val snippet =
        """
          |<input type="radio" checked="checked">
          |""".stripMargin

      Radio(Jsoup.parse(snippet).body().children().first()).checked shouldBe true
    }

    "return true if there is just a 'checked' property without value" in {
      val snippet =
        """
          |<input type="radio" checked>
          |""".stripMargin

      Radio(Jsoup.parse(snippet).body().children().first()).checked shouldBe true
    }

    "return false if there is no 'checked' property" in {
      val snippet =
        """
          |<input type="radio">
          |""".stripMargin

      Radio(Jsoup.parse(snippet).body().children().first()).checked shouldBe false
    }
  }
}
