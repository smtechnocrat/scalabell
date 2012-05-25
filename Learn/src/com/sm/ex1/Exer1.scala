/**
 *
 */
package com.sm.ex1

/**
 * @author smazumder
 *
 */

/**
 * Class definition in Scala is almost the same as Java except that classes 
 * can have parameters 
 * These arguments must be passed when creating an instance of
 * class Exer1, as follows: new Exer(1.5, 2.3).
 * 
 */
class Exer1 (real: Double, imaginary: Double ) {
  
  /**
   * One can access these class variables in two ways
   * 
   */
  def re = real
  def im = imaginary

}

object Exer1 {

  /**
   * The following function definition shows Scala's support for
   * first class functions.
   * functions are also objects in Scala.
   * It is therefore possible to pass functions as arguments, to store them in variables,
   * and to return them from other functions. This ability to manipulate functions as
   * values is one of the cornerstone of a very interesting programming paradigm called
   * functional programming.
   * 
   */
  def oncePerSecond(callback: () => Unit) {
    while (true)
      callback();
      Thread sleep 1000
  } 
  
  def timeFlies() = {
    println("time flies like an arrow.")
  }
  
  /**
   * Anonymous functions
   * Instead od declaring the timeflies function, we can construct this 
   * function just as it is passed on to oncePerSecond function.
   * 
   */
  def main(Args: Array[String]) {
       //oncePerSecond(timeFlies)
    
    oncePerSecond(() => println("Time flies like an arrow."))
  }
}