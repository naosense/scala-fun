package play

import scala.util.{ Failure, Success, Try }

object ParserCombinator {

  def theLetterA(input: String): Try[(String, Unit)] = {
    input.toList match {
      case first :: rest if first == 'a' => Success((rest.toString(), ()))
      case _                             => Failure(ParseError(input))
    }
  }

  def matchLiteral(expected: String): Parser[Unit] = {
    (input: String) =>
      {
        input.take(expected.length) match {
          case s if s == expected => Success((input.drop(expected.length), ()))
          case _                  => Failure(ParseError(input))
        }
      }
  }

  def identifier(input: String): ParseResult[String] = {
    val matched = new StringBuilder()

    input.toList match {
      case first :: rest if first.isLetter => matched.append(first)
      case _                               => return Failure(ParseError(input))
    }

    matched.append(input.drop(1).takeWhile(c => c.isLetterOrDigit || c == '-'))

    val nextIndex = matched.length;
    Success((input.drop(nextIndex), matched.toString()))
  }

  def pair[R1, R2](parser1: Parser[R1], parser2: Parser[R2]): Parser[(R1, R2)] = {
    (input: String) =>
      {
        parser1(input) match {
          case Success((nextInput, result1)) =>
            parser2(nextInput) match {
              case Success((finalInput, result2)) => Success((finalInput, (result1, result2)))
              case Failure(ex)                    => Failure(ex)
            }
          case Failure(ex) => Failure(ex)
        }
      }
  }

  def map[A, B](parser: Parser[A], fn: (A) => B): Parser[B] = {
    (input: String) =>
      {
        parser(input) match {
          case Success((nextInput, result)) => Success((nextInput, fn(result)))
          case err @ Failure(_)             => err.asInstanceOf[ParseResult[B]]
        }
      }
  }

  // Tuples cannot be directly destructured in method or function parameters.
  def left[R1, R2](parser1: Parser[R1], parser2: Parser[R2]): Parser[R1] = {
    map(pair(parser1, parser2), (r: (R1, R2)) => r._1)
  }

  // Tuples cannot be directly destructured in method or function parameters.
  def right[R1, R2](parser1: Parser[R1], parser2: Parser[R2]): Parser[R2] = {
    map(pair(parser1, parser2), (r: (R1, R2)) => r._2)
  }

  def oneOrMore[A](parser: Parser[A]): Parser[Vector[A]] = {
    (input: String) =>
      {
        var result = Vector[A]()
        var remain = input
        parser.parse(remain) match {
          case Success((nextInput, firstItem)) => {
            remain = nextInput
            result :+= firstItem
            var break = false
            while (!break) {
              parser.parse(remain) match {
                case Success((nextInput, nextItem)) => {
                  remain = nextInput
                  result :+= nextItem
                }
                case Failure(exception) => break = true
              }
            }
            Success((remain, result))
          }
          case err @ Failure(exception) => err.asInstanceOf[ParseResult[Vector[A]]]
        }
      }
  }

  def zeroOrMore[A](parser: Parser[A]): Parser[Vector[A]] = {
    (input: String) =>
      {
        var result = Vector[A]()
        var remain = input

        var break = false
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

  type ParseResult[Output] = Try[(String, Output)]

  trait Parser[Output] extends (String => ParseResult[Output]) {
    def parse(input: String): ParseResult[Output] = {
      this(input)
    }
  }

  case class ParseError(input: String) extends Throwable
  case class Element(name: String, attributes: Vector[(String, String)], children: Vector[Element])
}
