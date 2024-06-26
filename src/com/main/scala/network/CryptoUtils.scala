package network

import java.security.spec.X509EncodedKeySpec
import java.security.KeyFactory
import java.security.PublicKey
import java.util.Base64
import javax.crypto.Cipher

object CryptoUtils {
  type SessionToken = String
  type EncryptedSessionToken = String
  type PubKey = String

  def main(args: Array[String]): Unit =
    print(create_session_token(pubkeytest))
  def create_session_token(
    pubKey: PubKey,
    existingToken: Option[SessionToken] = None
  ): (SessionToken, EncryptedSessionToken) = {
    // beware this code is a bit brittle due to having to tightly
    // align formatting over serialized data.
    val rsaKeyFactory = KeyFactory.getInstance("RSA")
    val bytes = Base64.getDecoder.decode(
      stripPublicKeyText(pubKey)
    )
    val rsaPublicKey: PublicKey =
      rsaKeyFactory.generatePublic(new X509EncodedKeySpec(bytes))
    val cipher = Cipher.getInstance("RSA")
    val public_key_object = rsaPublicKey
    cipher.init(Cipher.ENCRYPT_MODE, public_key_object)
    val session_token = existingToken.getOrElse(generate_random_session_token())
    val encrypted_session_token_bytes =
      cipher.doFinal(
        Base64.getDecoder.decode(
          Base64.getEncoder.encode(session_token.getBytes())
        )
      )
    val encodedMessage =
      Base64.getEncoder().encodeToString(encrypted_session_token_bytes)
    (session_token, encodedMessage)
  }

  def stripPublicKeyText(keyText: String): String = {
    val res = keyText.stripMargin
      .replace("\n", "")
      .replace("\\n", "")
      .replace("-----BEGIN PUBLIC KEY-----", "")
      .replace("-----END PUBLIC KEY-----", "")
      .replace("\"", "")
    res
  }

  def generate_random_session_token(): SessionToken =
    (0 to 64).foldLeft("") { (a, c) =>
      a + (scala.math.random() * 16).floor.toInt.toHexString
    }

  def stripCertText(certText: String): String =
    certText.stripMargin
      .replace("\n", "")
      .replace("-----BEGIN CERTIFICATE-----", "")
      .replace("-----END CERTIFICATE-----", "")

  def stripPrivateKeyText(keyText: String): String =
    keyText.stripMargin
      .replace("\n", "")
      .replace("-----BEGIN PRIVATE KEY-----", "")
      .replace("-----END PRIVATE KEY-----", "")

  var pubkeytest: String =
    """
      |-----BEGIN PUBLIC KEY-----
      |MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCwDDDZP6a4hCxPb6hBcT4VXKac
      |N9GgGZY3YBaAOcLqW+lUwPhPnbAXqidMyELNj7E93P8TGiFnok8sjK2LkuNXl/U9
      |hEboNAw6HOx1qgQn2H82bU1xzgz+aS8I73ot5LyFpxDuD7VSYoCWO/EtJNCv84x9
      |WozW2VTPoEgwtWGd+QIDAQAB
      |-----END PUBLIC KEY-----
      |""".stripMargin

}
