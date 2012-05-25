/**
 *  http://apocalisp.wordpress.com/2010/04/21/a-proper-constant-function-in-scala/
 *  http://code.google.com/p/scalaz/
 *  https://github.com/robey/kestrel/tree/master/src/main/scala/net/lag/kestrel
 */
package com.sm.objs

/**
 * @author smazumder
 *
 */
class Rational(n: Int, d: Int) {// Class variables

  require(d != 0) // express constraints on default class variables. Any method call inside the class declaration
                  // gets added to the primary constructor. Here the require check is added to the primary constructor
  
  val Ccon = 12;  // constant declaration in scala
  
  private val g = gcd(n.abs,d.abs)  // declare private class variable, not a constant
    
  var numer = n /g ;
  var denom = d /g ;

  def this(n: Int) = this(n, 1)

  override def toString() = "Rational:" + numer + "/" + denom

  /**
   * Addition method for Rational. Operator overloading is built-in 
   */
  def + (that: Rational): Rational = {
    
    new Rational( numer * that.denom + that.numer * denom,
                  denom * that.denom) 
    
  }
  
  def + (i: Int) : Rational = new Rational(numer + i * denom, denom)
  
  def * (that: Rational): Rational = {
    new Rational(numer * that.numer, denom * that.denom)
  } 
    
  
  private def gcd(a: Int, b: Int): Int  = {
    if (b == 0) a else gcd(b, a % b)
  }

}

object Main {
  def main(args: Array[String]) = {
    val p = 7
    val q = new Rational(2,3)
    println( q + p)
  }
}
 
  