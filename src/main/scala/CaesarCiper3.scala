import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

object CaesarCiper3 {
  def Encryption(text: String, shift: Int): String = {
    val encryptedStr = new StringBuilder
    for (c <- text) {
      var noOfShift = shift % 26
      val newShiftedChar = (c + noOfShift).toChar
      if (c.isLower) {
        if (newShiftedChar <= 'a' || newShiftedChar >= 'z') {
          noOfShift = noOfShift - ('z' - c) - 1
          encryptedStr.append(('a' + noOfShift).toChar)
        }
        else encryptedStr.append(newShiftedChar)
      }
      else if (c.isUpper) {
        if (newShiftedChar <= 'A' || newShiftedChar >= 'Z') {
          noOfShift = noOfShift - ('Z' - c) - 1
          encryptedStr.append(('A' + noOfShift).toChar)
        }
        else encryptedStr.append(newShiftedChar)
      }
      else encryptedStr.append(c)
    }
    var result = encryptedStr.toString
    result
  }

  def Decryption(text: String, shift: Int): String = {
    val decryptedStr = new StringBuilder
    for (c <- text) {
      var noOfShifts = shift % 26
      val newShiftedChar = (c - noOfShifts).toChar
      if (c.isLower) {
        if (newShiftedChar < 'a' || newShiftedChar > 'z') {
          noOfShifts = noOfShifts - (c - 'a') - 1
          decryptedStr.append (('z' - noOfShifts).toChar)
        }
        else decryptedStr.append(newShiftedChar)
      }
      else if (c.isUpper) {
        if (newShiftedChar < 'A' || newShiftedChar > 'Z') {
          noOfShifts = noOfShifts - (c - 'A') - 1
          decryptedStr.append(('Z' - noOfShifts).toChar )
        }
        else decryptedStr.append(newShiftedChar)
      }
      else decryptedStr.append(c)
    }
    var result = decryptedStr.toString
    result
  }

  def Cipher(str: String, shifts: Int, function: (String, Int) => String): String = {
    function(str, shifts)
  }

  def main(args: Array[String]): Unit = {
    printf("Enter a string:")
    var str = readLine()
    printf("Enter number of shifts:")
    var shifts = readInt()
    var encResult = Cipher(str, shifts, Encryption)
    println("Encrypted String is: " + encResult)
    var decResult = Cipher(encResult, shifts, Decryption)
    println("Decrypted String is: " + decResult)

  }


}
