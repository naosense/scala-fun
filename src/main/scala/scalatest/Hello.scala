package scalatest

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

object Hello extends App {
  val p = new Person("Alvin Alexander")
  println(s"Hello ${p.name}")

  val pro = Promise[Int]

}

class Person(var name: String)





