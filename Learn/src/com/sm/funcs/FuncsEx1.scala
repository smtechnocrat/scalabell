/**
 *
 */
package com.sm.funcs

/**
 * @author smazumder
 *
 */
object FuncsEx1 {
  
  def echo(args: String*) = {
    for ( arg <- args) println(arg)
  }

  def main(args: Array[String]) = {
    println("Hello world, how are you.", "today")
    var increase = (x:Int) => x + 1
    println(increase(10))
    
    val someNumbers = List(-11, -10, -4, 0, 4, 10, 11)
    someNumbers.foreach((x: Int) => println(x))
    someNumbers.filter((x:Int) => (x > 0))
    
  }
}