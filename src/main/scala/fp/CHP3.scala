package fp

import scala.annotation.tailrec

object CHP3 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }
  }

  import List.sum

  // ex3.1
  val x: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // ex3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // ex3.3
  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(x, xs)
  }

  // ex3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    n match {
      case 0 => l
      case _ =>
        l match {
          case Nil => Nil
          case Cons(_, xs) => drop(xs, n - 1)
        }
    }
  }

  // ex3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }
  }

  def main(args: Array[String]): Unit = {
    println(List("a", "b"))
    println(x)
    println(drop(List(1, 2, 3, 4, 5), 2))
    println(dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 3))
  }
}
