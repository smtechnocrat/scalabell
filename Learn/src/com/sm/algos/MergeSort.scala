/**
 *
 */
package com.sm.algos

/**
 * Merge sort is based on the divide-and-conquer paradigm. Its worst-case running time has a lower order of growth than insertion sort.
 * Since we are dealing with subproblems, we state each subproblem as sorting a subarray A[p .. r]. Initially, p = 1 and r = n, but these
 * values change as we recurse through subproblems.
 *
 * To sort A[p .. r]:
 *
 * 1. Divide Step
 *
 * 			If a given array A has zero or one element, simply return; it is already sorted.
 *          Otherwise, split A[p .. r] into two subarrays A[p .. q] and A[q + 1 .. r], each containing about half of the elements of A[p .. r].
 *          That is, q is the halfway point of A[p .. r].
 *
 *
 * 2. Conquer Step
 *
 * 			Conquer by recursively sorting the two subarrays A[p .. q] and A[q + 1 .. r].
 *
 * 3. Combine Step
 *
 * 			Combine the elements back in A[p .. r] by merging the two sorted subarrays A[p .. q] and A[q + 1 .. r] into a sorted sequence.
 *          To accomplish this step, we will define a procedure MERGE (A, p, q, r).
 *
 * Note that the recursion bottoms out when the subarray has just one element, so that it is trivially sorted.
 *
 * Analyzing Merge Sort
 * ------------------------
 * For simplicity, assume that n is a power of 2 so that each divide step yields two subproblems, both of size exactly n/2.
 *
 * The base case occurs when n = 1.
 *
 * When n ≥ 2, time for merge sort steps:
 *
 * Divide: Just compute q as the average of p and r, which takes constant time i.e. Θ(1).
 *
 * Conquer: Recursively solve 2 subproblems, each of size n/2, which is 2T(n/2).
 *
 * Combine: MERGE on an n-element subarray takes Θ(n) time.
 *
 * Summed together they give a function that is linear in n, which is Θ(n). Therefore, the recurrence for merge sort running time is
 *
 * Solving the Merge Sort Recurrence
 *
 * T(n) = Θ(n log n).
 *
 * Reminder: log n stands for log2 n.
 *
 * Compared to insertion sort [Θ(n2) worst-case time], merge sort is faster. Trading a factor of n for a factor of lg n is a good deal.
 * On small inputs, insertion sort may be faster. But for large enough inputs, merge sort will always be faster, because its running time
 * grows more slowly than insertion sorts.
 *
 * Reference: http://www.sorting-algorithms.com/
 *
 */
object MergeSort {

  //  def mergeSort(xs: List[Int]): List[Int] = {
  //    // Define the merge function that will be used to merge the two sorted arrays.
  //    // It takes two lists and returns a sorted list 
  //    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
  //
  //      case (Nil, _) => ys
  //      case (_, Nil) => xs
  //      case (x :: xt, y :: yt) => if (x <= y) x :: merge(xt, ys)
  //      else y :: merge(xs, yt)
  //    }
  //    // determine the starting divide point to split
  //    val n = xs.length / 2
  //    if (n == 0) xs
  //    else {
  //      // Divide the given list into two sub lists
  //      val (subList1, subList2) = xs.splitAt(n)
  //      // perform the combine step after the two sublists have been mergeSorted i.e called recursively
  //      merge(mergeSort(subList1), mergeSort(subList2))
  //    }
  //  }
  
  // A more generic mergeSort function that can be used for any type T
  def mergeSort[T](less: (T, T) => Boolean)(xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xt, y :: yt) => if (less(x, y)) x :: merge(xt, ys)
      else y :: merge(xs, yt)
    }
    // determine the starting divide point to split
    val n = xs.length / 2
    if (n == 0) xs
    else {
      // Divide the given list into two sub lists
      val (subList1, subList2) = xs.splitAt(n)
      // perform the combine step after the two sublists have been mergeSorted i.e called recursively
      merge(mergeSort(less)(subList1), mergeSort(less)(subList2))
    }

  }
  def main(args: Array[String]): Unit = {}
  println(mergeSort((x: Int, y: Int) => x < y)(List(32, 12, 67, 23, 8, 1, 6, 34, 87, 24, 10, 63)))
}