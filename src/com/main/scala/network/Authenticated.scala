package network

import zio.ExitCode
import zio.IO
import zio.Ref

trait Authenticated[PUBKEY, ENC] {
  def authenticated: IO[AuthenticationError, Boolean]
  //CREATE SESSION BY ENCRYPTING SECRET WITH THE PUBLIC KEY WE ARE TRYING TO AUTHENTICATE.
  //WE ALSO PROVIDE A PUBKEY TO ENCRYPT THE VERIFICATION RESPONSE
  def session(key: PUBKEY): IO[AuthenticationError, (ENC, PUBKEY)]
  //VERIFY SESSION BY DECRYPTING CYPHERTEXT; IF PLAINTEXT CONTAINS THE SECRET WE ARE AUTHENTICATED

  def verifySession(cyphertext: ENC): IO[AuthenticationError, Boolean]
}
trait AuthenticationError
object Authenticated {}
