package fp

import fp.CHP3.List.foldRight

import scala.annotation.tailrec

object CHP3 {

  sealed trait List[+A] {
    override def toString: String = {
      @tailrec
      def go(list: List[A], sb: StringBuilder): Unit = {
        list match {
          case Nil => sb.deleteCharAt(sb.length() - 1); sb.append(']')
          case Cons(head, tail) => sb.append(head).append(','); go(tail, sb)
        }
      }

      if (this eq Nil) "[]"
      else {
        val sb = new StringBuilder("[")
        go(this, sb)
        sb.toString()
      }
    }
  }
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

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
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

  // ex3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  // ex3.7
  // 不会，会先展开，再运算，短路数为O(n)

  // ex3.8，返回原列表
  foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _))

  // ex3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  // ex3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // ex3.11
  def sum2(ints: List[Int]): Int = {
    foldLeft(ints, 0)(_ + _)
  }

  def product2(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)(_ * _)
  }

  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc, _) => acc + 1)
  }

  // ex3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((xs, x) => Cons(x, xs))
  }

  // ex3.14
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    //foldLeft(reverse(a1), a2)((a2, x1) => Cons(x1, a2))
    foldRight(a1, a2)((x1, a2) => Cons(x1, a2))
  }

  // ex3.16
  def inc(lt: List[Int]): List[Int] = {
    lt match {
      case Nil => Nil
      case Cons(x, xs) => Cons(1 + x, inc(xs))
    }
  }

  // ex3.17
  def stringy(lt: List[Double]): List[String] = {
    lt match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, stringy(xs))
    }
  }

  // ex3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  }

  def main(args: Array[String]): Unit = {
    println(List("a", "b"))
    println(x)
    println(drop(List(1, 2, 3, 4, 5), 2))
    println(dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 3))
    println(init(List(1, 2, 3, 4, 5)))
    println(foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)))
    println(reverse(List(1, 2, 3, 4)))
    println(append(List(1, 2, 3), List(4, 5)))
    println(inc(List(1, 2, 3)))
  }
}
