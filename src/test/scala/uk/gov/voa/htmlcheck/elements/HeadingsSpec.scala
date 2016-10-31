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
import org.jsoup.nodes.Element
import uk.gov.voa.htmlcheck.HtmlCheckError

class H1Spec extends HtmlElementSpec[H1] {

  lazy val tagName = "h1"

  lazy val elementWrapper: (Element) => HtmlCheckError Xor H1 = H1.elementWrapper

  lazy val elementApply: (Element) => H1 = H1.apply
}

class H2Spec extends HtmlElementSpec[H2] {

  lazy val tagName = "h2"

  lazy val elementWrapper: (Element) => HtmlCheckError Xor H2 = H2.elementWrapper

  lazy val elementApply: (Element) => H2 = H2.apply
}

class H3Spec extends HtmlElementSpec[H3] {

  lazy val tagName = "h3"

  lazy val elementWrapper: (Element) => HtmlCheckError Xor H3 = H3.elementWrapper

  lazy val elementApply: (Element) => H3 = H3.apply
}

class H4Spec extends HtmlElementSpec[H4] {

  lazy val tagName = "h4"

  lazy val elementWrapper: (Element) => HtmlCheckError Xor H4 = H4.elementWrapper

  lazy val elementApply: (Element) => H4 = H4.apply
}
