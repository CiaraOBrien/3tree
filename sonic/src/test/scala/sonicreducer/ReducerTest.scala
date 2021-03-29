package sonicreducer

import org.scalatest.{*, given}
import org.scalatest.funsuite.AnyFunSuite
import scala.language.adhocExtensions

class ReducerTest extends AnyFunSuite {

  val test = Array(1, 2, 3, 4)

  test("Array-optimized API (Reducer.reduceArray)") {
    assert(             Reducer.reduceArray(Array.empty[Int])(_ + _).isEmpty)
    assert(1        === Reducer.reduceArray(Array(1))        (_ + _).get)
    assert(test.sum === Reducer.reduceArray(test)            (_ + _).get)
  }

  test("Generic API (Reducer.reduce)") {
    assert(             Reducer.reduce(Array.empty[Int])(_ + _).isEmpty)
    assert(1        === Reducer.reduce(Array(1))        (_ + _).get)
    assert(test.sum === Reducer.reduce(test)            (_ + _).get)
  }

  test("Stateful API (Reducer.apply)") {
    val r0 = Reducer[Int](_ + _)
    assert(-1 === r0.resultOrElse(-1))

    intercept[NoSuchElementException] {
      Reducer[Int](_ + _).result
    }

    val r1 = Reducer[Int](_ + _)
    r1(1)
    assert(1 == r1.result)

    val r4 = Reducer[Int](_ + _)
    test.foreach(r4)
    assert(test.sum === r4.resultOrElse(-1))
  }
  
}