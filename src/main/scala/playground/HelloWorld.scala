package playground

import scala.concurrent.Promise

object Hello extends App {
  val p = new Person("Alvin Alexander")
  println(s"Hello ${p.name}")

  val pro = Promise[Int]
}

class Person(var name: String)
