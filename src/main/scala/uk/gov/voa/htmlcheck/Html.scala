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

import cats.data.Xor
import cats.data.Xor._
import org.jsoup.Jsoup
import uk.gov.voa.htmlcheck.elements.{ContainerElement, ElementAttribute, HtmlElement}

import scala.language.{implicitConversions, reflectiveCalls}

case class Html(body: String)
  extends HtmlElement
    with ContainerElement {

  lazy val element = Jsoup.parse(body)

  lazy val heading: HtmlCheckError Xor String = element.getElementsByTag("h1") match {
    case elements if elements.isEmpty => Left(ElementOfTypeNotFound("h1"))
    case elements => Right(elements.first().text().trim)
  }

  lazy val title = element.title
}

object Html {

  object Implicits extends Implicits

  trait Implicits
    extends HtmlElement.Implicits
      with ElementAttribute.Implicits {

    implicit class ContentWrapper(content: {def body: String}) {
      val asHtml = Html(content.body)
    }

    implicit def convertToHtml(html: {def body: String}): Html = Html(html.body)

    implicit def addXorOps[E](xor: Xor[HtmlCheckError, E]): XorOps[E] = new XorOps[E](xor)

    class XorOps[E](xor: Xor[HtmlCheckError, E]) {
      def getOrError = xor.valueOr(error => throw error)
    }

  }

}
