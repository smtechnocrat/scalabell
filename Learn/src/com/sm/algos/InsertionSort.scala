/**
 *
 */
package com.sm.algos

/**
 * Implement insertion sort.
 * <pre>
 *
 * Input is a list of integers A1, A2....An
 * </pre>
 * <post>
 * return a list of integers such that A1 <= A2 <= A3 ... <= An
 * </post>
 *
 * Insertion sort algorithm - description
 * ----------------------------------------
 * Works like sorting a deck of cards manually where a user picks a card from a table on which the
 * cards are laid face side down and then inserting it into his left hand by doing the following
 * - compare the card that is on his right hand with each card starting from the right such that if
 * card on the right is smaller than the current card on the right then insert the card before the
 * current
 * card.
 *
 * ALGORITHM - Pseudo code
 *
 * for j <- 2 to length[A]
 * do key <- A[j]
 *
 * i <- j - 1
 *
 * while i > 0 and A[i] > key
 *
 * do A[i + 1] <- A[i]
 *
 * i <- i - 1
 *
 *
 * A[i + 1] <- key
 *
 * References: https://gist.github.com/1073659
 * References: http://www.sorting-algorithms.com/
 *
 */
object InsertionSort {

  /**
   * Trying to use foldRight function
   *
   */
  def insertionSort(ls: List[Int]): List[Int] = ls match {
    case Nil => Nil
    case x :: y => insert(x, insertionSort(y))
  }

  /**
   * A simple function that takes an integer and a list as arguments
   * and returns a list with the given integer inserted in the right sequence.
   */
  def insert(x: Int, ls: List[Int]): List[Int] = {
    if (ls.isEmpty || x <= ls.head) x :: ls
    else
      ls.head :: insert(x, ls.tail)
  }

  // Implementing insert using pattern matching 
  def insert1(x: Int, ls: List[Int]): List[Int] = ls match {
    case Nil => List[Int](x)
    case h :: t if x < h => x :: insert1(h, t)
    case h :: t => h :: insert1(x, t)
  }

  /**
   * Implementation of iSort using pattern matching
   *
   */
  def iSort(ls: List[Int]): List[Int] = ls match {
    case Nil => Nil
    case h :: t => insert1(h, iSort(t))
  }

  /**
   * Implementation of iSort using foldRight.
   *
   */
  def iSortfR(ls: List[Int]) = {
    (ls :\ List[Int]())((a, b) => insert1(a, b))
  }

  def main(args: Array[String]): Unit = {
    println(insertionSort(List(32, 12, 67, 23, 8)))
  }

}