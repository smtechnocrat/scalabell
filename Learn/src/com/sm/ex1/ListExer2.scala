/**
 *
 */
package com.sm.ex1

/**
 * @author smazumder
 *
 */
class ListExer2 {
}

object ListExer2 {

  def append[T](xs: List[T], ys: List[T]): List[T] =
    xs match {
      case Nil => ys
      case x :: xs => x :: append(xs, ys)
    }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Modified run-length encoding.
  // Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into 
  // the result list. Only elements with duplicates are transferred as (N, E) terms.
  // Example:
  //
  //	scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //	res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def main(Args: Array[String]): Unit = {
      println(append(List(1,2,3,5,6),List(4,5,6))(2))
  }

}