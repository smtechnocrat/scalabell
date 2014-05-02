/**
 *
 */
package com.sm.util.strings

/**
 * @author smdeveloper
 *
 */
object StringUtils {

  implicit class StringUtils(s: String) {

    //
    //Reverses a string of words. 
    // A string.reverse reverses the order of the words in the string as well as the chars in each word.
    // Therefore we need to reverse each element in the list and build out a string.   
    //
    def reverseWords() = {
      var res = s.reverse.split(" ").toList
      (res map (_.reverse)).mkString(" ")
    }

    def increment() = s.map(c => (c + 1).toChar)
    
    val MOD_ADLER = 65521
    
    def adler32checksum() = {
      var a =1 
      var b =0
      s.getBytes().foreach{ char =>
         a = (char + a) %  MOD_ADLER
         b = ( b + a) % MOD_ADLER        
      }
      //Int is 32 bits
      b * 65536 + a  // or (b << 16) + a
    }
  }

  def main(args: Array[String]): Unit = {

    val res = "HAL".increment

    val test = "My name is Scala strings."
      
    val speech = """four square and
      #seven "years" old.""".stripMargin('#').replaceAll("\n", " " )
    
    println(speech)
      
    println(test.reverseWords)
    println(res)

    println(test.adler32checksum)
  }

}