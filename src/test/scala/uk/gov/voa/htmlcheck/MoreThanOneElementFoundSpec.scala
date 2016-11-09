package uk.gov.voa.htmlcheck

import uk.gov.voa.htmlcheck.elements.ElementAttribute.IdAttribute
import uk.gov.voa.htmlcheck.tooling.UnitSpec

class MoreThanOneElementFoundSpec extends UnitSpec {

  "message" should {

    "say 'There are 2 elements found but only one 'h2' was expected' when there is no attribute given" in {
      MoreThanOneElementFound(2, "h2").message shouldBe "There are 2 elements found but only one 'h2' was expected"
    }

    "say 'There are 2 elements found but only one 'h2' having IdAttribute=id was expected' when there is id attribute given" in {
      MoreThanOneElementFound(2, "h2", IdAttribute("id")).message shouldBe "There are 2 elements found but only one 'h2' having IdAttribute=id was expected"
    }
  }
}
