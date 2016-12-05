package uk.gov.voa.htmlcheck.elements

import cats.data.Xor
import cats.data.Xor.{Left, Right}
import org.jsoup.nodes.Element
import uk.gov.voa.htmlcheck.{ElementOfWrongType, HtmlCheckError}
import uk.gov.voa.htmlcheck.elements.ElementAttribute.{IdAttribute, TypeAttribute}

import scala.language.implicitConversions

case class InputHidden(protected val element: Element) extends Input

object InputHidden {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor InputHidden =
    if (element.tagName() != "input" || !TypeAttribute(element).exists(_.value == "hidden"))
      Left(ElementOfWrongType("input-hidden", s"${element.tagName()}${TypeAttribute(element).map(t => s"-$t").getOrElse("")}", IdAttribute(element)))
    else
      Right(InputHidden(element))

}
