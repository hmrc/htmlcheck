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
import uk.gov.voa.htmlcheck.tooling.UnitSpec
import uk.gov.voa.htmlcheck.{AttributeNotFound, ElementOfWrongType, HtmlCheckError}

trait HtmlElementSpec[E <: HtmlElement] extends UnitSpec {

  def tagName: String

  def elementWrapper: Element => HtmlCheckError Xor E

  def elementApply: Element => E

  "elementWrapper" should {

    s"successfully instantiate from a '$tagName' html tag" in {
      val snippet =
        s"""
           |<$tagName />
           |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      elementWrapper(element) shouldBe Right(elementApply(element))
    }

    s"return ElementWithIdOfWrongType when instantiated from a non '$tagName' html tag" in {
      val snippet =
        """
          |<other />
          |""".stripMargin

      val element = Jsoup.parse(snippet).body().children().first()

      elementWrapper(element) shouldBe Left(ElementOfWrongType(tagName, "other", Left(AttributeNotFound(AttributeName("id")))))
    }
  }

}
