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

import cats.data.Xor._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import uk.gov.voa.htmlcheck.AttributeNotFound
import uk.gov.voa.htmlcheck.elements.A.HrefAttribute
import uk.gov.voa.htmlcheck.elements.ElementAttribute.Implicits._
import uk.gov.voa.htmlcheck.elements.ElementAttribute._
import uk.gov.voa.htmlcheck.tooling.UnitSpec
import uk.gov.voa.htmlcheck.tooling.generators.GeneratorOf

class XorElementAttributeOpsSpec extends UnitSpec with PropertyChecks {

  private val attributes: Gen[ElementAttribute] = for {
    value <- GeneratorOf.nonEmptyString
    attribute <- Gen.oneOf(Seq(
      CustomAttribute(AttributeName(s"attribute-$value"), Some(value)),
      CustomAttribute(AttributeName(s"no-value-attribute-$value"), None),
      IdAttribute(s"id-$value"),
      ValueAttribute(s"value-$value"),
      NameAttribute(s"name-$value"),
      ClassAttribute(s"class-$value"),
      TagAttribute(s"tag-$value"),
      HrefAttribute(s"href-$value")
    ))
  } yield attribute

  "asString" should {

    "return attribute's toString if it exists" in {
      forAll(attributes.map(right)) {
        case maybeAttribute@Right(attribute) =>
          maybeAttribute.asString shouldBe attribute.toString
        case _ => fail("Wrong test configuration")
      }
    }

    "return an empty String if it's None" in {
      Left(AttributeNotFound(AttributeName("id"))).asString shouldBe ""
    }
  }

  "present" should {

    "return true if there's an attribute" in {
      forAll(attributes.map(right)) {
        case maybeAttribute@Right(attribute) =>
          maybeAttribute.present shouldBe true
        case _ => fail("Wrong test configuration")
      }
    }

    "return false if there's no an attribute" in {
      Left(AttributeNotFound(AttributeName("id"))).present shouldBe false
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

class CustomAttributeSpec extends UnitSpec {

  import uk.gov.voa.htmlcheck.Html.Implicits._

  private val snippet =
    """<div id="div" attribute-with-value="value" attribute-without-value />
      |""".toHtml

  private val div = snippet.findFirstDescendantOfType[Div].getOrError

  "apply" should {

    "return Right of CustomAttribute if there's an attribute and it has some value" in {
      div.attribute(AttributeName("attribute-with-value")) shouldBe Right(CustomAttribute(AttributeName("attribute-with-value"), value = Some("value")))
    }

    "return Right of CustomAttribute if there's an attribute and it has no value" in {
      div.attribute(AttributeName("attribute-without-value")) shouldBe Right(CustomAttribute(AttributeName("attribute-without-value"), value = None))
    }

    "return Left if there is no queries attribute" in {
      div.attribute(AttributeName("non-existing-attribute")) shouldBe Left(AttributeNotFound(AttributeName("non-existing-attribute")))
    }
  }
}