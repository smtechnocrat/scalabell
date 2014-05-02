/**
 *
 */
package com.sm.util.strings

import util.control.Breaks._
import scala.annotation.tailrec

/**
 * @author smdeveloper
 *
 */
object SCControl {

  def main(args: Array[String]): Unit = {

    val nieces = List("emily", "varick", "sabrina", "cleo", "maureen", "romola")
    println(for (n <- nieces) yield n.capitalize)

    for (i <- 0 until nieces.length) {
      println(s"$i is ${nieces(i)}")
    }

    breakable {
      for ((e, count) <- nieces.zipWithIndex) {
        if (count > 3) break
        println(s"$count is $e")
        
      }
    }
    
    val searchMe = "peter piper picked a peck of pickled peppers"
     
    val count = searchMe.count(_ == 'p')
    println(count)
    
    def fac(n: Int) : Int = {
      @tailrec
      def facAcc(Acc: Int, n: Int) : Int = {
         if (n <= 1) Acc
         else
           facAcc(n * Acc, n -1)
      }
      facAcc(1,n)
    }
    
    println(fac(10))

  }

}