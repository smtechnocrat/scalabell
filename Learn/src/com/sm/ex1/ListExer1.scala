/**
 *
 */
package com.sm.ex1

/**
 * @author smazumder
 *
 */
class ListExer1 {

}
object ListExer1 {
 
  /**
   *  Find the last element of a list.
   *		Example:
   *			scala> last(List(1, 1, 2, 3, 5, 8))
   *				   res0: Int = 8
   *
   * Note: The `[A]` allows us to handle lists of any type.
   */
  def last[A](ls: List[A]) = {
    ls.reverse.head
  }
  
  /**
   * A recursive implementation of last.
   * 
   */
  def lastR[A](ls: List[A]): A = ls match {
    case h :: Nil   => h
    case _ :: tail  => lastR(tail)
    case _          => throw new NoSuchElementException
    
  }
  
  /** Final implementation of last using Builtins */
  def lastBuiltin[A](ls :List[A]): A = ls.last
  
  //////////////////////////////////////////////////////
  // Last but one element of a list.
  ///////////////////////////////////////////////////////
  
  /**
   * Find the last but one element of a list.
   * Example:
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   */
  def penultimate[A](ls : List[A]) = {
    ls.reverse.tail.head
  }
  
  def penultimateR[A](ls : List[A]) : A = ls match {
    case h :: _ :: Nil  =>  h
    case _ :: tail   =>  penultimateR(tail)
    case _           => throw new NoSuchElementException
  } 
  
  /** 
   * Using rhe Builtin function this time. 
   * 
   * Note : ls.init function returns the list without the last element.
   *   val abcde = List('a', 'b', 'c', 'd', 'e')
   *   abced.init = List('a', 'b', 'c', 'd')
   */
  def penultimateBuiltin[A](ls :List[A]) : A = 
    if (ls.isEmpty) throw new NoSuchElementException
    else ls.init.last
  
  /////////////////////////////////////////////////////////
  // Generic lastNth element of a list.
  //
  // lastNth(3, List(1,2,2,5,6,7,8)) = 6
  //
  ////////////////////////////////////////////////////////
  def lastNthBuiltin[A](n:Int, ls: List[A]) : A = {
      if (n <= 0 ) throw new IllegalArgumentException
      if (ls.length < n) throw new IllegalArgumentException
      ls.takeRight(n).head
    } 
  /**
   * Finally a recursive definition of lastNth
   */
  def lastNthElementR[A](n: Int, ls : List[A]): A = {
    def lastNthR(count : Int, resultList : List[A], currList : List[A]) : A = currList match {
      case Nil if (count > 0) => throw new NoSuchElementException
      case Nil                => resultList.head
      case _ :: tail          =>  lastNthR( count -1, 
                                            if (count > 0) resultList else resultList.tail, tail)
    }
    if (n <= 0) throw new IllegalArgumentException
    else lastNthR(n,ls,ls)
    
  }
 
  //////////////////////////////////////////////////////////////////////////
  // Find the number of elements of a list. 
  // Example: 
  //  scala> length(List(1, 1, 2, 3, 5, 8))
  //  res0: Int = 6
  //
  ///////////////////////////////////////////////////////////////////////////

  def lengthN[A](ls: List[A]): Int = ls match {
    case Nil => 0
    case _ :: tail => 1 + lengthN(tail)
  }

  /**
   * Tail recursive solution for lengthN
   * Tail recursive solution.  Theoretically more efficient; with tail-call
   * elimination in the compiler, this would run in constant space.
   * Unfortunately, the JVM doesn't do tail-call elimination in the general
   * case.  Scala *will* do it if the method is either final or is a local
   * function.  In this case, `lengthR` is a local function, so it should
   * be properly optimized.
   * For more information, see
   * http://blog.richdougherty.com/2009/04/tail-calls-tailrec-and-trampolines.html
   */
  def lengthTR[A](ls: List[A]): Int = {
    def lengthR(curLs: List[A], res: Int): Int = curLs match {
      case Nil => res
      case _ :: tail => lengthR(tail, res + 1)
    }
    lengthR(ls, 0)
  }

  /** Pure functional solution using foldLeft */

  def lengthFun[A](ls: List[A]): Int =
    ls.foldLeft(0) { (c, _) => c + 1 }

 //////////////////////////////////////////////////////////////
  // Reverse a list. 
  //Example: 
  //scala> reverse(List(1, 1, 2, 3, 5, 8))
  //	res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  /////////////////////////////////////////////////////////////

  def reverseR[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    //Adds the elements of the tail of given list in front of a new list with head
    case h :: tail => reverseR(tail) ::: List(h) //
  }

  /** A tail recursive form of reverseR */
  def reverseT[A](ls: List[A]): List[A] = {
    def reverseTailRecursive(currList: List[A], res: List[A]): List[A] = currList match {
      case Nil => res
      case h :: tail => reverseTailRecursive(tail, h :: res)
    }
    reverseTailRecursive(ls, Nil)
  }

  /** Pure functional definition. */
  def reverseL[A](ls: List[A]): List[A] =
    ls.foldLeft(List[A]()) { (r, h) => h :: r }

  /////////////////////////////////////////////////
  // Find out whether a list is a palindrome. 
  //  Example: 
  //    scala> isPalindrome(List(1, 2, 3, 2, 1))
  //     res0: Boolean = true
  //////////////////////////////////////////////////
  /**
   * In theory, we could be slightly more efficient than this.  This approach
   * traverses the list twice: once to reverse it, and once to check equality.
   * Technically, we only need to check the first half of the list for equality
   * with the first half of the reversed list.  The code to do that more
   * efficiently than this implementation is much more complicated, so we'll
   * leave things with this clear and concise implementation
   */
  def isPalindrome[A](ls: List[A]): Boolean = (ls.reverse == ls)

     /////////////////////////////////////////////////////////////
  // 
  // Flatten a list structure
  // scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  //   res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  //
  // DO NOT UNDERSTAND THIS SOLUTION
  //////////////////////////////////////////////////////////////
  def flattenR(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flattenR(ms)
    case e => List(e)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////
  //  Eliminate consecutive duplicates of list elements. 
  // If a list contains repeated elements they should be replaced with a single copy of the element. 
  // The order of the elements should not be changed. 
  // Example:
  //
  //scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  ////////////////////////////////////////////////////////////////////////////////////////////////////
  
  def compressR[A](ls: List[A]) : List[A] = ls match {
    case Nil => Nil
    case h::tail  => h :: compressR(tail.dropWhile( _== h)) 
  }
  
  /**
   * Tail recursive implementation of compress
   */
  def compressTR[A](ls : List[A]): List[A] = {
    def compressTailRecursive(xs: List[A], result : List[A]) : List[A] = xs match {
      case Nil => result.reverse  //since the elements added first will be at the end of the list.
      case h :: tail => compressTailRecursive(tail.dropWhile( _== h), h::result)
    }
    compressTailRecursive(ls, Nil)    
  }
  /**
   * Pure functional implementation of compress.
   */
  def compressF[A](ls: List[A]) : List[A] = 
     ls.foldRight(List[A]()){ (h,r) =>
       if(r.isEmpty || r.head !=h) h::r
       else r }
 
  ////////////////////////////////////////////////////////////////////////////////////////////////////
  //  Pack consecutive duplicates of list elements into sublists. 
  //  If a list contains repeated elements they should be placed in separate sublists. 
  // Example:
  //
  //scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  ////////////////////////////////////////////////////////////////////////////////////////////////////
  
  def pack[A](xs : List[A]): List[List[A]] = {
    if (xs.isEmpty) List(List())
    else {
      val (packed, next) = xs span { _ == xs.head}
      if ( next == Nil) List(packed)
      else packed :: pack(next)
    }
  }
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//Run-length encoding of a list.
	//Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
	//Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
	//Example:
	//
	//scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	//res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def encode[A](xs : List[A]) : List[(Int, A)] = {
    pack(xs) map { e => (e.length, e.head)}
  }
  
  
  
  def main(args : Array[String]) : Unit = {
    //    println((List(1, 1, 2, 3, 5, 8).last))
    //    println(NthElementBuiltin(5, List(1, 1, 2, 3, 5, 8)))
    //    println(lengthTR(List(1, 1, 2, 3, 5, 8)))
    //    println(reverseT(List(1, 1, 2, 3, 5, 8)))
    //    println(isPalindrome(List(1, 2, 3, 2, 1)))
    //    println(flattenR(List(List(1, 1), 2, List(3, List(5, 8)))))
    //    println(compressTR(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    //    println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  }
}