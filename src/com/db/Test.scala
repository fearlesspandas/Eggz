package src.com.db
import java.security.SecureRandom
import scala.util.Random
import BigInt._

/*
 * Reference: https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange
 *
 * Note: x.modPow(y,z) = x^y mod z
 */
object DiffieHellmannKeyExchange {
  // Either Alice or Bob generates a prime p and a random base g (which are both public)
  val p = BigInt(32, 100, new SecureRandom)       //> p  : scala.math.BigInt = 13597010238923887999
  val g = Random.nextInt(100)                     //> g  : Int = 16

  // Random private key generation
  val a = Random.nextInt(100) // Private key Alice//> a  : Int = 17
  val b = Random.nextInt(100) // Private key Bob  //> b  : Int = 93

  // Public key generation
  val A = g.modPow(a, p) // Public key Alice      //> A  : scala.math.BigInt = 9610690161951177877
  val B = g.modPow(b, p) // Public key Bob        //> B  : scala.math.BigInt = 9154186299771118507

  // Now Alice and Bob both can generate the common secret by using each others public key and their own private key
  val sAlice = B.modPow(a, p)                     //> sAlice  : scala.math.BigInt = 11113420747582081577
  val sBob = A.modPow(b, p)                       //> sBob  : scala.math.BigInt = 11113420747582081577

  sAlice == sBob

  //> res0: Boolean = true

  def main(args:Array[String]):Unit = {
    print(sAlice,sBob,a,b,A,B,p,g)
  }
}