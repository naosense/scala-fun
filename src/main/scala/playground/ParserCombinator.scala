package playground

import scala.util.{Failure, Success, Try}

object ParserCombinator {

  def theLetterA(input: String): Try[(String, Unit)] = {
    input.toList match {
      case first :: rest if first == 'a' => Success((rest.mkString, ()))
      case _ => Failure(ParseError(input))
    }
  }

  def matchLiteral(expected: String): Parser[Unit] = {
    (input: String) => {
      input.take(expected.length) match {
        case s if s == expected => Success((input.drop(expected.length), ()))
        case _ => Failure(ParseError(input))
      }
    }
  }

  def identifier(input: String): ParseResult[String] = {
    val matched = new StringBuilder()

    input.toList match {
      case first :: _ if first.isLetter => matched.append(first)
      case _ => return Failure(ParseError(input))
    }

    matched.append(input.drop(1).takeWhile(c => c.isLetterOrDigit || c == '-'))

    val nextIndex = matched.length;
    Success((input.drop(nextIndex), matched.toString()))
  }

  def pair[R1, R2](parser1: Parser[R1], parser2: Parser[R2]): Parser[(R1, R2)] = {
    (input: String) => {
      parser1(input) match {
        case Success((nextInput, result1)) =>
          parser2(nextInput) match {
            case Success((finalInput, result2)) => Success((finalInput, (result1, result2)))
            case Failure(ex) => Failure(ex)
          }
        case Failure(ex) => Failure(ex)
      }
    }
  }

  def map[A, B](parser: Parser[A], fn: (A) => B): Parser[B] = {
    (input: String) => {
      parser(input) match {
        case Success((nextInput, result)) => Success((nextInput, fn(result)))
        case err@Failure(_) => err.asInstanceOf[ParseResult[B]]
      }
    }
  }

  // Tuples cannot be directly destructured in method or function parameters.
  def left[R1, R2](parser1: Parser[R1], parser2: Parser[R2]): Parser[R1] = {
    map(pair(parser1, parser2), (r: (R1, R2)) => r._1)
  }

  // ditto
  def right[R1, R2](parser1: Parser[R1], parser2: Parser[R2]): Parser[R2] = {
    map(pair(parser1, parser2), (r: (R1, R2)) => r._2)
  }

  def oneOrMore[A](parser: Parser[A]): Parser[Vector[A]] = {
    map(pair(parser, zeroOrMore(parser)), { case (head: A, tail: Vector[A]) => head +: tail })
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

  def anyChar(input: String): ParseResult[Char] = {
    input.toList match {
      case first :: rest => Success((rest.mkString, first))
      case _ => Failure(ParseError(input))
    }
  }

  def pred[A](parser: Parser[A], predicate: A => Boolean): Parser[A] = {
    (input: String) => {
      parser(input) match {
        case Success((nextInput, value)) if predicate(value) => Success((nextInput, value))
        case _ => Failure(ParseError(input))
      }
    }
  }

  def whitespaceChar(): Parser[Char] = {
    pred(anyChar, c => c.isWhitespace)
  }

  def space1(): Parser[Vector[Char]] = {
    oneOrMore(whitespaceChar())
  }

  def space0(): Parser[Vector[Char]] = {
    zeroOrMore(whitespaceChar())
  }

  def quotedString(): Parser[String] = {
    map(
      right(
        matchLiteral("\""),
        left(
          zeroOrMore(pred(anyChar, (c: Char) => c != '"')),
          matchLiteral("\"")
        )
      ),
      (chars: Vector[Char]) => chars.mkString
    )
  }

  def attributePair(): Parser[(String, String)] = {
    pair(identifier, right(matchLiteral("="), quotedString()))
  }

  def attributes(): Parser[Vector[(String, String)]] = {
    zeroOrMore(right(space1(), attributePair()))
  }

  def elementStart(): Parser[(String, Vector[(String, String)])] = {
    right(matchLiteral("<"), pair(identifier, attributes()))
  }

  def singleElement(): Parser[Element] = {
    map(
      left(elementStart(), matchLiteral("/>")),
      { case (name, attributes) =>
        Element(name, attributes, Vector())
      }
    )
  }

  type ParseResult[Output] = Try[(String, Output)]

  trait Parser[Output] extends (String => ParseResult[Output]) {
    def parse(input: String): ParseResult[Output] = {
      this (input)
    }

  }

  case class ParseError(input: String) extends Throwable
  case class Element(name: String, attributes: Vector[(String, String)], children: Vector[Element])
}
