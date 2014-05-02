/**
 *
 */
package com.sm.datastructures

/**
 * @author smdeveloper
 *
 */
sealed trait List[+A] {
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    
    def sum(xs: List[Int]): Int = xs match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
    
    def product(xs:List[Double]) : Double = xs match {
      case Nil => 1.0
      case Cons(0.0,_) => 0.0
      case Cons(x, y) => x * product(y)
    }
    
    def apply[A](xs: A*) : List[A] = {
      if (xs.isEmpty) Nil
      else Cons(xs.head, apply(xs.tail: _*))
    }
    
    def tail[A](xs: List[A]) : List[A] = xs match {
      case Nil => Nil
      case Cons(x,y) => y
    }
    
    def drop[A](xs: List[A], n: Int) : List[A] = {
      if (n == 0) xs
      else drop(tail(xs), n -1)
    }
    
    def dropWhile[A](xs: List[A],f: A => Boolean) : List[A] = xs match {
      case Nil => Nil
      case Cons(x,ls)  if (f(x)) => dropWhile(ls,f)
      case _ => xs
    }
    
    def setHead[A](xs:List[A], r: A) : List[A] = xs match {
      case Nil => Cons(r, Nil)
      case Cons(x, ls)  => Cons(r,ls) 
      
    }
  }
}