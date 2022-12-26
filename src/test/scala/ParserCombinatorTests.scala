import playground.ParserCombinator._

import scala.util.{Failure, Success}

class ParserCombinatorTests extends munit.FunSuite {

  test("literal parser") {
    val parseJoe = matchLiteral("Hello Joe!")
    assertEquals(parseJoe("Hello Joe!"), Success("", ()))
  }

  test("identifier parser") {
    assertEquals(identifier("i-am-an-identifier"), Success(("", "i-am-an-identifier")))
    assertEquals(identifier("not entirely an identifier"), Success((" entirely an identifier", "not")))
    assertEquals(identifier("!not at all an identifier"), Failure(ParseError("!not at all an identifier")))
  }

  test("pair combinator") {
    val tagOpener = pair(matchLiteral("<"), identifier _)
    assertEquals(tagOpener("<my-first-element/>"), Success(("/>", ((), "my-first-element"))))
    assertEquals(tagOpener("oops"), Failure(ParseError("oops")))
    assertEquals(tagOpener("<!oops"), Failure(ParseError("!oops")))
  }

  test("right combinator") {
    val tagOpener = right(matchLiteral("<"), identifier _)
    assertEquals(tagOpener("<my-first-element/>"), Success(("/>", "my-first-element")))
    assertEquals(tagOpener("oops"), Failure(ParseError("oops")))
    assertEquals(tagOpener("<!oops"), Failure(ParseError("!oops")))
  }

  test("one or more combinator") {
    val parser = oneOrMore(matchLiteral("ha"))
    assertEquals(parser("hahaha"), Success(("", Vector((), (), ()))))
    assertEquals(parser("ahah"), Failure(ParseError("ahah")))
    assertEquals(parser(""), Failure(ParseError("")))
  }

  test("zero or more combinator") {
    val parser = zeroOrMore(matchLiteral("ha"))
    assertEquals(parser("hahaha"), Success(("", Vector((), (), ()))))
    assertEquals(parser("ahah"), Success(("ahah", Vector.empty)))
    assertEquals(parser(""), Success("", Vector.empty))
  }

  test("predicate combinator") {
    val parser = pred(anyChar, (c: Char) => c == 'o')
    assertEquals(parser("omg"), Success(("mg", 'o')))
  }

  test("quoted string parser") {
    assertEquals(quotedString().parse("\"Hello Joe!\""), Success("", "Hello Joe!"))
  }

  test("attribute parser") {
    assertEquals(attributes().parse(" one=\"1\" two=\"2\""), Success("", Vector(("one", "1"), ("two", "2"))))
  }

  test("single element parser") {
    assertEquals(
      singleElement().parse("<div class=\"float\"/>"),
      Success((
        "",
        Element("div", Vector(("class", "float")), Vector()))
      )
    )
  }

  test("xml parser") {
    val doc =
      """
        |<top label="Top">
        |    <semi-bottom label="Bottom"/>
        |    <middle>
        |        <bottom label="Another bottom"/>
        |    </middle>
        |</top>
        |""".stripMargin

    val parseDoc = Element(
      "top",
      Vector(("label", "Top")),
      Vector(
        Element("semi-bottom", Vector(("label", "Bottom")), Vector()),
        Element("middle", Vector(), Vector(Element("bottom", Vector(("label", "Another bottom")), Vector())))
      )
    )
    assertEquals(element().parse(doc), Success(("", parseDoc)))
  }
}
