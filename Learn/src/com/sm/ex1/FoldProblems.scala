/**
 *
 */
package com.sm.ex1

/**
 * @author smazumder
 *
 */
object FoldProblems {

  // given a list of integers. calculate its sum using foldLeft
  def sum(xs: List[Int]): Int = (0 /: xs)(_ + _)

  //given a list of integers. calculate its product using foldLeft
  def prod(xs: List[Int]): Int = (1 /: xs)(_ * _)

  //Define a flatten method that concatenates all elements in a list of list using foldLeft
  def flatten[T](xss: List[List[T]]) =
    (List[T]() /: xss)(_ ::: _)

  //Define a flatten method that concatenates all elements in a list of list using foldRight
  def flattenRight[T](xss: List[List[T]]) =
    (xss :\ List[T]())(_ ::: _)

  //Reverse a list of integers using foldLeft
  def reverseLeft(xss: List[Int]): List[Int] =
    (List[Int]() /: xss) { (ys, y) => y :: ys }

  //Reverse a list of integers using foldRight
  def reverseRight(xss: List[Int]): List[Int] =
    (xss :\ List[Int]()) { (y, ys) => ys ::: List(y) }

  //Given a List[A] return the last value in the list. Again, no using List’s last() method.
  def last[T](xss: List[T]) =
    (xss.head /: xss) { (_, y) => y }

  // Write a function called ‘penultimate’ that takes a List[A] and returns the penultimate item 
  //(i.e. the next to last item) in the list. Hint: Use a tuple.
  def penultimate[T](xss: List[T]) =
     ((xss.head, xss.tail.head) /: xss){ (r,c) => (r._2, c)}._1
  
  def main(args: Array[String]): Unit = {

    println(penultimate(List(32, 12, 67, 23, 8, 1, 6, 34, 87, 24, 10, 63)))
    //println(flatten(List( List(32, 12, 67), List(23, 8, 1, 6, 34),List(87, 24, 10, 63))))
  }

}