package com.sebnozzi.ninetynine

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.Matchers

class NinetyNineProblemsSuite extends FunSuite with Matchers {

  import NinetyNineProblems._

  test("P01 - should find the last element of a list") {
    last(List(1, 2, true, "five", 8)) should be(8)
    last(List(7)) should be(7)
  }

  test("P02 - find the penultimate element of a list") {
    penultimate(List(1, 2, 3, 5, 8)) should be(5)
    penultimate(List(1, 3)) should be(1)
  }

  test("P03 - find the Nth element of a list") {
    nth(3, List(1, 1, 3, 5, 8)) should be(5)
    nth(0, List(1, 1, 3, 5, 8)) should be(1)
  }

  test("P04 - find the number of elements of a list") {
    import NinetyNineProblems.{ length => myLength }  // conflicts with Matchers :-(
    myLength(List(1, 1, 2, 3, 5, 7)) should be(6)
    lengthFolding(List(1, 1, 2, 3, 5, 7)) should be(6)
  }

  test("P05 - reverse a list") {
    def reverse[T](list: List[T]): List[T] = {
      def helper(remaining: List[T], inverted: List[T]): List[T] =
        remaining match {
          case Nil => inverted
          case first :: rest => helper(rest, first :: inverted)
        }
      helper(list, Nil)
    }

    def reverseFolding[T](list: List[T]): List[T] =
      list.foldLeft(List.empty[T])((result, element) => element :: result)

    reverse(List(1, 2, 3, 4)) should be(List(4, 3, 2, 1))
    reverseFolding(List(1, 2, 3, 4)) should be(List(4, 3, 2, 1))
  }
  
  test("P06 - find out whether a list is palyndrome") {
    isPalyndrome(List(1,2,3)) should be (false)
    isPalyndrome(List(1,2,2,1)) should be (true)
    isPalyndrome(List(5)) should be (true)
  }

}
