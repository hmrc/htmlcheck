package uk.gov.voa.htmlcheck.elements

import cats.data.Xor
import cats.data.Xor.{Left, Right}
import org.jsoup.nodes.Element
import uk.gov.voa.htmlcheck.elements.ElementAttribute.{IdAttribute, TypeAttribute}
import uk.gov.voa.htmlcheck.{ElementOfWrongType, HtmlCheckError}

import scala.language.implicitConversions

case class InputFile(protected val element: Element) extends Input

object InputFile {

  implicit def elementWrapper(element: Element): HtmlCheckError Xor InputFile =
    if (element.tagName() != "input" || !TypeAttribute(element).exists(_.value == "file"))
      Left(ElementOfWrongType("input-file", s"${element.tagName()}${TypeAttribute(element).map(t => s"-$t").getOrElse("")}", IdAttribute(element)))
    else
      Right(InputFile(element))

}
