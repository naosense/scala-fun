package fp

import scala.annotation.tailrec

object CHP2 {
  // ex2.1
  def fib(n: Int): Int = {
    assert(n > 0)

    @tailrec
    def iter(count: Int, curr: Int, next: Int): Int = {
      count match {
        case 1 => curr
        case _ => iter(count - 1, next, curr + next)
      }
    }

    iter(n, 0, 1)
  }

  // ex2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n - 1), as(n))) false
      else loop(n + 1)
    }

    loop(1)
  }

  // ex2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => b: B => f(a, b)
  }

  // ex2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // ex2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(fib(5))
    println(isSorted(Array(1, 2, 3, 3), (a: Int, b: Int) => a <= b))
  }
}
