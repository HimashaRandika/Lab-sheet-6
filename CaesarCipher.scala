package Caesar_Cipher.CaesarCipher

object CaesarCipher {

  def main(args: Array[String]): Unit = {
    val plaintext = scala.io.StdIn.readLine("Enter the String -: ")
    val shift = scala.io.StdIn.readLine("Enter the number of shifts you want -: ").toInt

    val encryptedString = caesarCipher(plaintext, shift, Encrypt)
    println("Encrypted -: " + encryptedString)

    val decryptedString = caesarCipher(encryptedString, shift, Decrypt)
    println("Decrypted -: " + decryptedString)
  }


  def Encrypt(plainText: String, shift: Int): String = {
    val alphabetSize = 26

    plainText.map { char_Input =>
      if (char_Input.isLetter)

      {
        val isUpperCase = char_Input.isUpper
        val base = if (isUpperCase) 'A' else 'a'
        val shiftedChar = ((char_Input - base + shift + alphabetSize) % alphabetSize + base).toChar

        if (isUpperCase) shiftedChar
        else shiftedChar.toLower
      }
      else {
        char_Input
      }
    }
  }


  def Decrypt(cipherString: String, shift: Int): String = {
    Encrypt(cipherString, -shift)
  }


  def caesarCipher(string: String, shift: Int, func: (String, Int) => String): String = {
    func(string, shift)
  }
}