/**
 *
 */
package com.sm.algos

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
    case x::xst => val (before, after) = xst.partition(_< x)
                   qsort(before) ::: x::qsort(after)
  }

  def main(args: Array[String]): Unit = {}
   println(qsort(List(32, 12, 67, 23, 8, 1, 6, 34, 87, 24, 10, 63)))
}