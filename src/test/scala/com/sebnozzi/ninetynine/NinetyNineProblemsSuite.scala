package com.sebnozzi.ninetynine

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers

@RunWith(classOf[JUnitRunner])
// }}
class NinetyNineProblemsSuite extends FunSuite with Matchers {
  
  test("P01 - should find the last element of a list") {
    def last[T](list: List[T]): T =
      list match {
        case x :: Nil => x
        case x :: rest => last(rest)
        case Nil => error("An empty list has no last element")
      }
    last(List(1, 2, 3, 5, 8)) should be(8)
    last(List(7)) should be(7)
  }

  test("P02 - find the penultimate element of a list") {
    def penultimate[T](list: List[T]): T =
      list match {
        case first :: last :: Nil => first
        case first :: second :: rest => penultimate(second :: rest)
        case _ => error("Can not find penultimate of this list: " + list)
      }

    penultimate(List(1, 2, 3, 5, 8)) should be(5)
    penultimate(List(1, 3)) should be(1)
  }

  test("P03 - find the Nth element of a list") {
    def nth[T](n: Int, list: List[T]): T =
      if (n == 0)
        list.head
      else
        nth(n - 1, list.tail)

    nth(3, List(1, 1, 3, 5, 8)) should be(5)
    nth(0, List(1, 1, 3, 5, 8)) should be(1)
  }

  test("P04 - find the number of elements of a list") {
    def length[T](list: List[T]): Int =
      list match {
        case Nil => 0
        case x :: rest => 1 + length(rest)
      }

    length(List(1, 1, 2, 3, 5, 7)) should be(6)
  }

  
}
