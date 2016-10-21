package uk.gov.voa.htmlcheck.elements

import cats.data.Xor.{Left, Right}
import org.jsoup.Jsoup
import uk.gov.voa.htmlcheck.ElementOfWrongType
import uk.gov.voa.htmlcheck.elements.Label.ForAttribute
import uk.gov.voa.htmlcheck.tooling.UnitSpec

class LabelSpec extends UnitSpec {

  "elementWrapper" should {

    "successfully instantiate from a 'label' html tag" in {
      val snippet =
        """
          |<label />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Label.elementWrapper(element) shouldBe Right(Label(element))
    }

    "return ElementWithIdOfWrongType when instantiated from a non 'label' html tag" in {
      val snippet =
        """
          |<div />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      Label.elementWrapper(element) shouldBe Left(ElementOfWrongType("label", "div", None))
    }
  }

  "forAttribute" should {

    "return ForAttribute if the 'for' is defined on the tag" in {
      val snippet =
        """
          |<label for="abc" />
          |""".stripMargin

      val element = Label(Jsoup.parse(snippet).body().children().first())

      element.forAttribute shouldBe Some(ForAttribute("abc"))
    }

    "return None for 'for' when the attribute is not defined on the tag" in {
      val snippet =
        """
          |<label />
          |""".stripMargin

      val element = Label(Jsoup.parse(snippet).body().children().first())

      element.forAttribute shouldBe None
    }

    "return None for 'for' when the attribute has no value" in {
      val snippet =
        """
          |<label for="" />
          |""".stripMargin

      val element = Label(Jsoup.parse(snippet).body().children().first())

      element.forAttribute shouldBe None
    }
  }

}
