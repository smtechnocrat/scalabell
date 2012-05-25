/**
 *
 */
package com.sm.algos

/**
 *  Sort by comparing each adjacent pair of items in a list in turn, swapping the items if necessary, and repeating the pass through the list until no swaps are done.
 *
 */
object BubbleSort {

  def bubblesort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case x::Nil => List(x)
    case (x :: y :: xst) => if (y < x) y::bubblesort(x::xst)
                            else x::bubblesort(y::xst)
  }

  def main(args: Array[String]): Unit = {}
    println(bubblesort(List(32, 12, 67, 23, 8, 1, 6, 34, 87, 24, 10, 63)))
}