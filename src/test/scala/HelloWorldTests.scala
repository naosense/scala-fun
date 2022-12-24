import org.scalatest.concurrent.ScalaFutures._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import play.Person

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class HelloWorldTests extends AnyFunSuite {

  // test 1
  test("the name is set correctly in constructor") {
    val p = new Person("Barney Rubble")
    assert(p.name == "Barney Rubble")
  }

  // test 2
  test("a Person's name can be changed") {
    val p = new Person("Chad Johnson")
    p.name = "Ochocinco"
    assert(p.name == "Ochocinco")
  }

  test("future") {
    val fut = Future { 21 + 21 }
    fut.futureValue should be (42)
  }
}