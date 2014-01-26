package com.sm.problems

object NinetyNineProblems {
  println("Welcome to the Scala worksheet")

  /**
   * returns the last element of the list.
   */
  def last(xs: List[Int]): Int = xs match {
    case Nil => 0
    case x :: Nil => x
    case x :: xl => last(xl)
  }

  /**
   * Find the last but one element of the list.
   */
  def penultimate(xs: List[Int]): Int = xs match {
    case Nil => -1
    case x :: Nil => -1
    case x :: y :: Nil => x
    case x :: xst => penultimate(xst)
  }

  /**
   * Find the kth element of the list.
   *
   */
  def nth(k: Int, xs: List[Int]): Int = (k, xs) match {
    case (0, x :: _) => x
    case (k, _ :: tail) => nth(k - 1, tail)
  }

  /**
   * find the number of elements in a list.
   */
  def length(xs: List[Int]): Int = xs match {
    case Nil => 0
    case x :: tail => 1 + length(tail)
  }

  /**
   * reverse the list.
   */
  def rev(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case (x :: tail) => rev(tail) ::: List(x)
  }
  /**
   *  A tail recursive implementation.
   */
  def revt(xs: List[Int]): List[Int] = {

    def reverse(acc: List[Int], xs: List[Int]): List[Int] = xs match {
      case Nil => acc
      case x :: tail => reverse(x :: acc, tail)
    }
    reverse(Nil, xs)
  }

  /**
   * return true if the list is a palindrome. Could be done more
   * effeciently by reversing the list first and only comparing for equality
   *  for first half of the list.
   *
   */
  def isPalindrome(xs: List[Int]): Boolean = xs == revt(xs)

  /**
   * Flatten a nested list structure.
   */
  def flatten(xs: List[Any]): List[Any] = xs flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)

  }

  /**
   *  eleminate consecutive duplicates from a list.
   */
   def compress(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case h :: tail => h :: compress(tail.dropWhile(_ == h))

  }
  
  /**
   * Pack consecutive duplicates of elements into sublists
   */
  def pack(xs:List[Any]): List[ List[Any ]] = xs match {
     case Nil => List(List())
     case x::tail => val (packed,next) = xs span { _ == x}
                      if (next == Nil) List(packed)
                      else packed:: pack(next)
  }
     
  def encodeModified(xs:List[Any]):List[(Int, Any)] = {
      pack(xs).map(e => (e.length,e.head))
  }
  
  /**
   * Given a run length code list generated as specified in the encode function, this
   * function constructs a uncompressed version.
   */
   def decode(xs: List[(Int,Any)]) : List[Any] = {
      xs flatMap { e => List.make(e._1, e._2) }
   }
  ////////////////////////////////////////////////////////////////////////////
  // Print results.
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  println(penultimate(List(3, 5, 7, 8, 9, 19, 3, 3, 3, 39, 8)))
  println(nth(5, List(3, 5, 7, 8, 9, 19, 3, 3, 3, 39, 8)))
  println(length(List(3, 5, 7, 8, 9, 19, 3, 3, 3, 39)))

  println(revt(List(3, 5, 7, 8, 9, 19, 3, 3, 3, 39)))
  println(isPalindrome(List(1, 2, 3, 2, 1)))
  println(flatten(List(List(1, 2, 3), 4, 5, List(6, 7, 8))))
  println(compress(List(2,3,4,4,6,7,8,9,9,9)))
  
  println(pack(List('a, 'a, 'a, 'a, 'b , 'c, 'c, 'c, 'c, 'd)))
                                                 
  println(encodeModified(List('a, 'a, 'a, 'a, 'b , 'c, 'c, 'c, 'c, 'd)))
  
  println(decode(List((4,'a), (1,'b), (4,'c), (1,'d))))

}