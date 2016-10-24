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

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import uk.gov.voa.htmlcheck.elements.A.HrefAttribute
import uk.gov.voa.htmlcheck.elements.ElementAttribute.Implicits._
import uk.gov.voa.htmlcheck.elements.ElementAttribute._
import uk.gov.voa.htmlcheck.tooling.UnitSpec
import uk.gov.voa.htmlcheck.tooling.generators.GeneratorOf

class ElementAttributeOpsSpec extends UnitSpec with PropertyChecks {


  private val attributes = for {
    value <- GeneratorOf.nonEmptyString
    attribute <- Gen.oneOf(Seq(
      GenericAttribute(AttributeName(s"attribute-$value"), value),
      IdAttribute(s"id-$value"),
      ValueAttribute(s"value-$value"),
      NameAttribute(s"name-$value"),
      ClassAttribute(s"class-$value"),
      TagAttribute(s"tag-$value"),
      HrefAttribute(s"href-$value")
    ))
  } yield attribute

  "asString" should {

    "return attribute's value if it's not None" in {
      forAll(attributes.map(Option.apply)) {
        case maybeAttribute@Some(attribute) =>
          maybeAttribute.asString shouldBe attribute.value
        case None => fail("Wrong test configuration")
      }
    }

    "return an empty String if it's None" in {
      None.asString shouldBe ""
    }

  }
}

class ImplicitSpec extends UnitSpec {

  "stringToClassAttributeWrapper" should {

    "convert a String into a ClassAttribute when needed" in {
      val sample = GeneratorOf.nonEmptyString.sample.get

      val value: ClassAttribute = sample

      value.value shouldBe sample
    }
  }

  "stringToIdAttributeWrapper" should {

    "convert a String into an IdAttribute when needed" in {
      val sample = GeneratorOf.nonEmptyString.sample.get

      val value: IdAttribute = sample

      value.value shouldBe sample
    }
  }

}