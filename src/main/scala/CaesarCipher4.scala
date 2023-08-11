import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

object CaesarCipher4 {

    def Encryption(text: String, shifts: Int): String = {
      val encryptedStr = new StringBuilder
      var ch = ' '
      for (c <- text) {
        if (c.isLetter) {
          ch = (c + shifts % 26).toChar
          encryptedStr.append(ch)
        }
        else {
          encryptedStr.append(c)
        }

      }
      var res = encryptedStr.toString
      res
    }

    def Decryption(text: String, shifts: Int): String = {
      val decryptedStr = new StringBuilder
      var ch = ' '
      for (c <- text) {
        if (c.isLetter) {
          ch = (c - shifts % 26).toChar
          decryptedStr.append(ch)
        }
        else {
          decryptedStr.append(c)
        }

      }
      var res = decryptedStr.toString
      res

    }

    def Ciper(str: String, noOfShifts: Int, function: (String, Int) => String): String = {
      function(str, noOfShifts)

    }

    def main(args: Array[String]): Unit = {

      printf("Enter a string:")
      var str = readLine()
      printf("Enter number of shifts:")
      var shifts = readInt()
      var enResult = Ciper(str, shifts, Encryption)
      println("Encrypted String is: " + enResult)
      var deResult = Ciper(enResult, shifts, Decryption)
      println("Decrypted String is: " + deResult)

    }



}
