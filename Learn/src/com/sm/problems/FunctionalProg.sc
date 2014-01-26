/**
 * A worksheet for all problems from the Functional Programming in Scala book.
 *
 *
 */
package com.sm.problems

object FunctionalProg {

  /**
   * A tail recursive factorial function
   */
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc:Int) : Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }
    go( n, 1)
  }                                               //> factorial: (n: Int)Int
  
  
  /**
   * Get the nth Fibonacci number. The first two fibonacci numbers are 0, 1
   * fib n = (n-1) + (n-2)
   */
  def fibSlow(n: Int) : Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fibSlow(n-1) + fibSlow(n-2)
  }                                               //> fibSlow: (n: Int)Int
  /**
   * Get the nth Fibonacci number. The first two fibonacci numbers are 0, 1
   * fib n = (n-1) + (n-2)
   */
  def fib(n: Long) : Long = {
    @annotation.tailrec
    def go(n:Long, prev: Long, curr: Long) : Long = {
      if ( n ==0 ) prev
      else go(n -1, curr, prev + curr)
    }
    go(n, 0, 1)
  }                                               //> fib: (n: Long)Long
    
 def formatRes(name: String, n: Int, f:Int => Int) = {
   val msg= "The %s of %d is %d."
   msg.format(n, f(n))
 }                                                //> formatRes: (name: String, n: Int, f: Int => Int)String
  
  
  def binarySearch(ds: Array[Double], lookfor: Double) : Int = {
  
     @annotation.tailrec
     def go(low: Int, mid: Int, high: Int) : Int = {
       if (low > high) -mid - 1
       else {
         val mid2 = (low + high) /2
         val d = ds(mid2)
         if ( lookfor == d) mid2
         else if ( d > lookfor) go(low, mid2, mid2-1)
         else go(mid2+1, mid2, high)
       }
     }
     go(0,0,ds.length-1)
  }                                               //> binarySearch: (ds: Array[Double], lookfor: Double)Int
  
  
  def binSearch[A](as: Array[A], key: A, gt: (A,A) => Boolean) : Int = {
     @annotation.tailrec
     def go(low: Int, mid:Int, high: Int) : Int = {
        if (low > high) - mid -1
        else {
          val mid2 = (low + high) / 2
          val d = as(mid2)
          
          val greater = gt(d, key)
          
          if (!greater && !gt(key,d)) mid2
          else if ( greater) go(low, mid2, mid2-1)
          else go(mid2+1, mid2, high)
          
        }
     }
   go(0,0,as.length-1)
  }                                               //> binSearch: [A](as: Array[A], key: A, gt: (A, A) => Boolean)Int
  
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean) : Boolean = {
    @annotation.tailrec
    def go(i: Int, prev: A): Boolean = {
       if ( i == as.length ) true
       else if (gt(as(i), prev)) go(i + 1, as(i))
       else false
    }
    if (as.length == 0) true
    else
      go(1, as(0))
  }                                               //> isSorted: [A](as: Array[A], gt: (A, A) => Boolean)Boolean
  
 
 /**
  * This partial function takes a value and a function that takes two arguments and
  * returns a function of one argument as its result.
  */
 def partial1[A,B,C](a: A, f: (A,B) => C) : B => C =
     (b:B) => f(a, b)                             //> partial1: [A, B, C](a: A, f: (A, B) => C)B => C
  
 
 /**
  * Needs more thought.
  */
 def curry[A,B,C](f:(A,B) => C) : A => (B => C) =
    a => b => f(a,b)                              //> curry: [A, B, C](f: (A, B) => C)A => (B => C)
  
  
  println("Welcome to the Scala Functional Programming Worksheet")
                                                  //> Welcome to the Scala Functional Programming Worksheet
  println("Fib of 4= " + fib(50))                 //> Fib of 4= 12586269025
  
  println("Using binSearch to find:" , binSearch(Array(2,5,6,78,34,26,29,11,14), 29, (x:Int, y:Int) => x < y))
                                                  //> (Using binSearch to find:,6)

  println(isSorted(Array(1,2,4,5,6), (x:Int, y:Int) => x > y))
                                                  //> true
  
}