
# htmlcheck

[![Build Status](https://travis-ci.org/hmrc/htmlcheck.svg?branch=master)](https://travis-ci.org/hmrc/htmlcheck) [ ![Download](https://api.bintray.com/packages/hmrc/releases/htmlcheck/images/download.svg) ](https://bintray.com/hmrc/releases/htmlcheck/_latestVersion)

This is a scala wrapper over jsoup to access html content in a functional way.

Some examples:

* this is how to traverse html using `flatMap`
```scala
import uk.gov.voa.htmlcheck.Html
import uk.gov.voa.htmlcheck.Html.Implicits._
import uk.gov.voa.htmlcheck.elements.ElementAttribute.IdAttribute
import uk.gov.voa.htmlcheck.elements.{Div, TextArea}

val html = Html(
  """<div id="div">
    | <textarea id="1" class="area-class"></textArea>
    | <p></p>
    | <div id="inner-div">
    |   <textarea id="2" class="area-class"></textArea>
    | </div>
    |</div>
    |""".stripMargin)

val textArea2 = html
  .findOnlyDescendantBy[IdAttribute, Div]("div")
  .flatMap(_.findFirstChildBy[IdAttribute, Div]("inner-div"))
  .flatMap(_.onlyChild[TextArea])

val textArea2Id = textArea2.getOrError.id.asString
```

* and using `for`
```scala
import cats.data.Xor.Right
import uk.gov.voa.htmlcheck.Html
import uk.gov.voa.htmlcheck.Html.Implicits._
import uk.gov.voa.htmlcheck.elements.ElementAttribute.IdAttribute
import uk.gov.voa.htmlcheck.elements.{Div, TextArea}

val html = Html(
  """<div id="div">
    | <textarea id="1" class="area-class"></textArea>
    | <p></p>
    | <div id="inner-div">
    |   <textarea id="2" class="area-class"></textArea>
    | </div> approach
    |</div>
    |""".stripMargin)


val Right((textArea2, id)) = for {
  div <- html.findOnlyDescendantBy[IdAttribute, Div]("div")
  innerDiv <- div.findFirstChildBy[IdAttribute, Div]("inner-div")
  textArea2 <- innerDiv.onlyChild[TextArea]
  id <- textArea2.id
} yield textArea2 -> id
```

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").
    