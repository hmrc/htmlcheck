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
import uk.gov.voa.htmlcheck.elements.AttributeName
import uk.gov.voa.htmlcheck.elements.ElementAttribute.IdAttribute
import uk.gov.voa.htmlcheck.tooling.UnitSpec

class ElementSiblingNotFoundSpec extends UnitSpec {

  "message" should {

    "say 'Element of type 'p' with IdAttribute=id not found' when both attribute and explanation given" in {

      val error = ElementSiblingNotFound("not found", "p", Right(IdAttribute("id")))

      error.message shouldBe "Element of type 'p' with IdAttribute=id not found"
    }

    "say 'Element of type 'p' not found' when explanation is given but no attribute" in {

      val error = ElementSiblingNotFound("not found", "p", Left(AttributeNotFound(AttributeName("id"))))

      error.message shouldBe "Element of type 'p' not found"
    }

  }

}

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
