/**
 *
 */
package com.sm.algos

import scala.annotation.tailrec

/**
 *   function quicksort('array')
 * 		if length('array') ≤ 1
 * 			return 'array'  // an array of zero or one elements is already sorted
 * 		select and remove a pivot value 'pivot' from 'array'
 * 		create empty lists 'less' and 'greater'
 * 		for each 'x' in 'array'
 * 			if 'x' ≤ 'pivot' then append 'x' to 'less'
 * 			else append 'x' to 'greater'
 * 	 return concatenate(quicksort('less'), 'pivot', quicksort('greater')) // two recursive calls
 *
 * References:
 *    http://xlinux.nist.gov/dads/
 *    http://flyingfrogblog.blogspot.com/
 *    http://vemod.net/purely-functional-heap-sort-in-scala
 */
object QuickSort {

  def qsort(xs: List[Int]): List[Int] = xs match {

    case Nil => Nil
    case x :: xst =>
      val (before, after) = xst.partition(_ < x)
      qsort(before) ::: x :: qsort(after)
  }

  /**
   * *
   * Quicksort implementation using an accumalator.
   *
   *
   */

  def qsort2(xs: List[Int]): List[Int] = {

    @tailrec def part_acc(x: Int, xs: List[Int], l: List[Int], e: List[Int], r: List[Int], Acc: List[Int]): List[Int] = xs match {
      case Nil => qsort_acc(l, e ::: qsort_acc(r, Acc))
      case h :: t if (h < x) =>
        part_acc(x, t, h :: l, e, r, Acc)
      case h :: t if (h > x) =>
        part_acc(x, t, l, e, h :: r, Acc)
      case h :: t if (h == x) =>
        part_acc(x, t, l, h :: e, r, Acc)

    }
    def qsort_acc(ls: List[Int], acc: List[Int]): List[Int] = ls match {
      case Nil => acc
      case (h :: t) => part_acc(h, t, List(), List(h), List(), acc)
    }

    qsort_acc(xs, List())
  }

  def main(args: Array[String]): Unit = {}
  println(qsort2(List(32, 12, 67, 23, 8, 1, 6, 34, 87, 24, 10, 63)))
}