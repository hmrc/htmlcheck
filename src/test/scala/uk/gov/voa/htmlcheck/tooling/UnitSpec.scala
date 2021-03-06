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

package uk.gov.voa.htmlcheck.tooling

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.scalatest.{Matchers, WordSpec}
import uk.gov.voa.htmlcheck.Html

trait UnitSpec extends WordSpec with Matchers {

  protected implicit class StringToHtmlConverter(html: String) {

    lazy val toHtml: Html = Html(html.stripMargin)

    lazy val parse: Element = Jsoup.parse(html.stripMargin)

  }
}
