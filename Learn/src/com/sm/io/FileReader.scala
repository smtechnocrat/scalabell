package com.sm.io

import scala.io.Source

object FileReader extends App {

  //  Console.println("Hello from Scala Application.")
  //  val greetStrings = new Array[String](3)
  //  greetStrings(0)= "Hello";
  //  greetStrings(1)= ", ";
  //  greetStrings(2)= "World!\n";
  //  
  //  for (i<-0 to 2)
  //    Console.print(greetStrings(i))

  /**
   * Not purely functional since it returns an Unit, it has a sideeffect, first printing to the console is one
   * and secondly if a a function does not return anything interesting which is an Unit type, the only way this
   * function can make a difference is by a sideeffect.
   */
  def printArgs(args: Array[String]): Unit = {
    args foreach println
  }

  // Pure functional form, no sideeffects
  def formatArgs(args: Array[String]): String = args.mkString("\n")
  
  //println(formatArgs(args))
  //assert(res =="Hello")
  
  def readFile(file : String): String = {
    Source.fromFile(file).getLines().toList.mkString("")
  }
  
  println(readFile(args(0)))
  println("task completed.")
}
