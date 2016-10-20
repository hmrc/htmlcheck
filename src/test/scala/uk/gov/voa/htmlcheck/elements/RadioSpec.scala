package uk.gov.voa.htmlcheck.elements

import org.jsoup.Jsoup
import uk.gov.voa.htmlcheck.tooling.UnitSpec

class RadioSpec extends UnitSpec {

  "selected" should {

    "return true if there is a 'selected' property with value of selected" in {
      val snippet =
        """
          |<input type="radio" selected="selected">
          |""".stripMargin

      Radio(Jsoup.parse(snippet).body().children().first()).selected shouldBe true
    }

    "return true if there is just a 'selected' property without value" in {
      val snippet =
        """
          |<input type="radio" selected>
          |""".stripMargin

      Radio(Jsoup.parse(snippet).body().children().first()).selected shouldBe true
    }

    "return false if there is no 'selected' property" in {
      val snippet =
        """
          |<input type="radio">
          |""".stripMargin

      Radio(Jsoup.parse(snippet).body().children().first()).selected shouldBe false
    }
  }
}
