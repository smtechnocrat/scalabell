/**
 *
 */
package com.sm.interview

/**
 * @author smazumder
 *
 */
class QuesPart1 {
  
 
}
object QuesPart1 {

   /**
   * Define a function that takes a string as input and then reverses
   * the words within it.
   * E.g. reverseWordsinStr("My Name is Suvankar Mazumder")
   *           = "Mazumder Suvankar is Name My")
   */
  
  def reverseW(str : String) = {
    var res = str.reverse.split(" ").toList
    (res.map(_.reverse)).mkString(" ")
  }

  def reverseWordsinStr(str: String) = {
    var resList = str.reverse.split(" ").toList  
    (resList map(_.reverse)).mkString(" ")
  }

  def main(Args: Array[String]): Unit = {
    println(reverseW("My Name is Suvankar Mazumder"));
  }
}