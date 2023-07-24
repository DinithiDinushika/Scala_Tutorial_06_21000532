import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
object CaesarCipher {

  def Encryption(text : String, shifts : Int): String = {
    val encryptedStr = new StringBuilder
    var ch , aAChar, zZChar = ' '
    for(c <- text){
      if(c.isLetter){
        if(c.isUpper){
          aAChar = 'A'
          zZChar = 'Z'
        }
        else{
          aAChar = 'a'
          zZChar = 'z'
        }
        ch = (c + shifts % 26).toChar
        if(ch.isLetter){
          encryptedStr.append(ch)
        }
        else{
          var extraMoves = ch - zZChar
          var newShiftedChar = (aAChar + extraMoves -1).toChar
          encryptedStr.append(newShiftedChar)

        }

      }
      else{
        encryptedStr.append(c)
      }

    }
    var res = encryptedStr.toString
    res
  }

  def Decryption(text: String, shifts : Int): String = {
    val decryptedStr = new StringBuilder
    var ch , aAChar, zZChar = ' '
    for (c <- text) {
      if(c.isLetter){
        if (c.isUpper) {
          aAChar = 'A'
          zZChar = 'Z'
        }
        else {
          aAChar = 'a'
          zZChar = 'z'
        }
        ch = (c - shifts % 26).toChar
        if (ch.isLetter) {
          decryptedStr.append(ch)
        }
        else {
          var extraMoves = ch - aAChar
          var newShiftedChar = (zZChar - extraMoves + 1).toChar
          decryptedStr.append(newShiftedChar)

        }
      }
      else{
        decryptedStr.append(c)
      }

    }
    var res = decryptedStr.toString
    res

  }

  def Ciper(str: String, noOfShifts : Int, function: (String, Int) => String): String = {
    function(str, noOfShifts)

  }
  def main(args: Array[String]): Unit = {

    printf("Enter a string:")
    var str = readLine()
    printf("Enter number of shifts:")
    var shifts = readInt()
    var enResult = Ciper(str, shifts, Encryption)
    println("Encrypted String is: " +enResult)
    var deResult = Ciper(enResult, shifts, Decryption)
    println("Decrypted String is: " + deResult)

  }


}
