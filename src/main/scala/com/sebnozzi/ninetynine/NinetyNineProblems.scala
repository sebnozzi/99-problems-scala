package com.sebnozzi.ninetynine

object NinetyNineProblems {

  // P01 - should find the last element of a list
  def last[T](list: List[T]): T =
    list match {
      case List(only) => only
      case first :: rest => last(rest)
      case Nil => error("An empty list has no last element")
    }

  // P02 - find the penultimate element of a list
  def penultimate[T](list: List[T]): T =
    list match {
      case List(first, last) => first
      case first :: second :: rest => penultimate(second :: rest)
      case _ => error("Can not find penultimate of this list: " + list)
    }

  // P03 - find the Nth element of a list
  def nth[T](n: Int, list: List[T]): T =
    if (n == 0)
      list.head
    else
      nth(n - 1, list.tail)

  // P04 - find the number of elements of a list (recursive)
  def length[T](list: List[T]): Int =
    list match {
      case Nil => 0
      case x :: rest => 1 + length(rest)
    }

  // P04 - find the number of elements of a list (folding)
  def lengthFolding[T](list: List[T]): Int =
    list.foldLeft(0)((count, _) => 1 + count)

  // P05 - reverse a list (recursive)
  def reverse[T](list: List[T]): List[T] = {
    def helper(remaining: List[T], inverted: List[T]): List[T] =
      remaining match {
        case Nil => inverted
        case first :: rest => helper(rest, first :: inverted)
      }
    helper(list, Nil)
  }

  // P05 - reverse a list (folding)
  def reverseFolding[T](list: List[T]): List[T] =
    list.foldLeft(List.empty[T])((result, element) => element :: result)

  // P06 - find out whether a list is palyndrome
  def isPalyndrome[T](list: List[T]) = reverse(list) == list

  // P07 - flatten a nested list structure
  def flatten(list: List[_]): List[_] = {
    def helper(remainder: Any, result: List[_]): List[_] =
      remainder match {
        case Nil => result
        case (head:List[_]) :: tail => helper(tail, result ++ flatten(head))
        case head :: tail => helper(tail, result :+ head)
      }
    helper(list, Nil)
  }

}
