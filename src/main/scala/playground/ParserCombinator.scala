package playground

import scala.util.{Failure, Success, Try}

object ParserCombinator {

  def theLetterA(input: String): Try[(String, Unit)] = {
    input.toList match {
      case first :: rest if first == 'a' => Success((rest.mkString, ()))
      case _ => Failure(ParseError(input))
    }
  }

  def literal(expected: String): Parser[Unit] = {
    (input: String) => {
      input.take(expected.length) match {
        case s if s == expected => Success((input.drop(expected.length), ()))
        case _ => Failure(ParseError(input))
      }
    }
  }

  def identifier(input: String): ParseResult[String] = {
    pair(
      (anychar _).pred(c => c.isLetter),
      zeroOrMore((anychar _).pred(c => c.isLetterOrDigit || c == '-'))
    ).parse(input) match {
      case Success((rest, (first, second))) =>
        Success((rest, first + second.mkString))
      case Failure(exception) => Failure(ParseError(input))
    }
  }

  def pair[R1, R2](parser1: Parser[R1], parser2: Parser[R2]): Parser[(R1, R2)] = {
    for {
      result1 <- parser1
      result2 <- parser2
    } yield (result1, result2)
  }

  def map[A, B](parser: Parser[A], fn: (A) => B): Parser[B] = {
    (input: String) => {
      parser.parse(input) match {
        case Success((nextInput, result)) => Success((nextInput, fn(result)))
        case err@Failure(_) => err.asInstanceOf[ParseResult[B]]
      }
    }
  }

  // Tuples cannot be directly destructured in method or function parameters.
  def left[R1, R2](parser1: Parser[R1], parser2: Parser[R2]): Parser[R1] = {
    pair(parser1, parser2).map({ case (l, _) => l })
  }

  // ditto
  def right[R1, R2](parser1: Parser[R1], parser2: Parser[R2]): Parser[R2] = {
    pair(parser1, parser2).map({ case (_, r) => r })
  }

  def oneOrMore[A](parser: Parser[A]): Parser[Vector[A]] = {
    pair(parser, zeroOrMore(parser)).map(
      { case (head: A, tail: Vector[A]) => head +: tail }
    )
  }

  def zeroOrMore[A](parser: Parser[A]): Parser[Vector[A]] = {
    (input: String) => {
      var result = Vector[A]()
      var remain = input
      var break  = false
      while (!break) {
        parser.parse(remain) match {
          case Success((nextInput, nextItem)) =>
            remain = nextInput
            result :+= nextItem
          case Failure(_) => break = true
        }
      }
      Success((remain, result))
    }
  }

  def anychar(input: String): ParseResult[Char] = {
    input.toList match {
      case first :: rest => Success((rest.mkString, first))
      case _ => Failure(ParseError(input))
    }
  }

  def pred[A](parser: Parser[A], predicate: A => Boolean): Parser[A] = {
    (input: String) => {
      parser.parse(input) match {
        case Success((nextInput, value)) if predicate(value) => Success((nextInput, value))
        case _ => Failure(ParseError(input))
      }
    }
  }

  def whitespace(): Parser[Char] = {
    (anychar _).pred(c => c.isWhitespace)
  }

  def space1(): Parser[Vector[Char]] = {
    oneOrMore(whitespace())
  }

  def space0(): Parser[Vector[Char]] = {
    zeroOrMore(whitespace())
  }

  def quotedString(): Parser[String] = {
    right(
      literal("\""),
      left(
        zeroOrMore((anychar: Parser[Char]).pred((c: Char) => c != '"')),
        literal("\"")
      )
    ).map((chars: Vector[Char]) => chars.mkString)
  }

  def attributePair(): Parser[(String, String)] = {
    pair(identifier, right(literal("="), quotedString()))
  }

  def attributes(): Parser[Vector[(String, String)]] = {
    zeroOrMore(right(space1(), attributePair()))
  }

  def elementStart(): Parser[(String, Vector[(String, String)])] = {
    right(literal("<"), pair(identifier, attributes()))
  }

  def singleElement(): Parser[Element] = {
    left(elementStart(), literal("/>"))
      .map({ case (name, attributes) => Element(name, attributes, Vector()) })
  }

  def openElement(): Parser[Element] = {
    left(elementStart(), literal(">"))
      .map({ case (name, attributes) => Element(name, attributes, Vector()) })
  }

  def either[A](parser1: Parser[A], parser2: Parser[A]): Parser[A] = {
    (input: String) => {
      parser1.parse(input) match {
        case ok@Success(_) => ok
        case _ => parser2.parse(input)
      }
    }
  }

  def element(): Parser[Element] = {
    wrap(either(singleElement(), parentElement()))
  }

  def parentElement(): Parser[Element] = {
    for {
      el <- openElement()
      children <- left(zeroOrMore(element()), closeElement(el.name))
    } yield el.copy(children = children)
  }

  def wrap[A](parser: Parser[A]): Parser[A] = {
    right(space0(), left(parser, space0()))
  }

  def closeElement(expected: String): Parser[String] = {
    right(
      literal("</"),
      left(identifier, literal(">"))
    ).pred(name => name == expected)
  }

  type ParseResult[Output] = Try[(String, Output)]

  @FunctionalInterface
  trait Parser[Output] {
    def parse(input: String): ParseResult[Output]

    def map[NewOutput](fn: Output => NewOutput): Parser[NewOutput] = {
      ParserCombinator.map(this, fn)
    }

    def pred(predicate: Output => Boolean): Parser[Output] = {
      ParserCombinator.pred(this, predicate)
    }

    def flatMap[NewOutput](fn: Output => Parser[NewOutput]): Parser[NewOutput] = {
      (input: String) => {
        this.parse(input) match {
          case Success((nextInput, result)) => fn(result).parse(nextInput)
          case err@Failure(_) => err.asInstanceOf[ParseResult[NewOutput]]
        }
      }
    }
  }

  implicit def function2parser[Output](f1: String => ParseResult[Output]): Parser[Output] = {
    (input: String) => f1.apply(input)
  }

  case class ParseError(input: String) extends Throwable
  case class Element(name: String, attributes: Vector[(String, String)], children: Vector[Element])
}
