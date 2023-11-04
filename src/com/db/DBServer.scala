package src.com.db
import java.security.spec.X509EncodedKeySpec
import java.security.KeyFactory
import java.security.PublicKey
import java.security.Security
import java.util.Base64

import javax.crypto.Cipher
import org.bouncycastle.jce.provider.BouncyCastleProvider
import zhttp.http.Http
import zhttp.http.Request
import zhttp.http.Response
import zhttp.http._
import zhttp.service.Server
import zio.App
import zio.ExitCode
import zio.URIO
import zio._
import zio.json._
import zio.console._
object DBServer extends App {

  type PubKey = String
  type SessionToken = String
  type EncryptedSessionToken = String
  type SessionMap = scala.collection.concurrent.Map[PubKey, SessionToken]
  type PlayerMap = Map[PubKey, Player]
  val session_map: SessionMap = scala.collection.concurrent.TrieMap()
  val player_map: PlayerMap = Map()

  val app2: Http[Console, Throwable, Request, Response] = for{
    ref <- Http.collectZIO[Request]{case _ => Ref.make(session_map)}
    s <- Http.collectHttp[Request]{case _ =>addNewSessionToken(ref)}
  }yield s
  implicit def chunktobytearr(c: Chunk[Byte]): Array[Byte] = c.toArray

  def addNewSessionToken(
    m: Ref[SessionMap]
  ): Http[Console, Throwable, Request, Response] = Http.collectZIO[Request] {
    case req =>
      req match {
        case Method.POST -> !! / "create_session" =>
          for {
            smap <- m.get

            _ <- putStrLn(smap.toString())
            pubkey <- req.getBodyAsString
            strippedpubkey = stripPublicKeyText(pubkey)
            //get session token as well as its cyphertext
            existingToken = smap.get(strippedpubkey)
            token = create_session_token(pubkey,existingToken)
            curr_tokens = smap.getOrElse(strippedpubkey, token._1)
            //persist session token secret server side
            //updatedsmap = smap.updated(pubkey, curr_tokens)
            _ = smap.update(strippedpubkey,curr_tokens)
            _ <- m.set(smap)
            _ <- putStrLn( smap.toString())
          } yield {
            //return the token cyphertext
            //in order to verify client calls
            //we must only ever return the cyphertext
            //so that we can ensure only the user who
            //has the private key will know the session id
            Response.text(token._2)
          }
        case Method.POST -> !! / "verify_session" =>
          for {
            body <- req.getBodyAsString
            json = body.fromJson[VerifySession]
//            _ <- putStrLn(body)
//            _ <- putStrLn(json.toString)

            smap <- m.get

            if json.isRight
            vses = json.right.get
            t = smap.get(stripPublicKeyText(vses.publickey))
            _ <- putStrLn(s"vsestoken ${vses.token}")
            resp = ValidSession(stripPublicKeyText(vses.publickey),t.exists(_ == vses.token))
            _ <- putStrLn(t.toString)
            _<- putStrLn(resp.toString)
          }yield {
              Response.text(resp.toJson)
          }
      }

  }
  //We create a session token that we keep secret on the server side, as well as that token
  //encrypted with the users pubkey
  def create_session_token(pubKey: PubKey,existingToken:Option[SessionToken] = None): (SessionToken, EncryptedSessionToken) = {
    //beware this code is a bit brittle due to having to tightly
    //align formatting over serialized data.
    val rsaKeyFactory = KeyFactory.getInstance("RSA")
    val bytes = Base64.getDecoder.decode(
      stripPublicKeyText(pubKey)
    )
    val rsaPublicKey: PublicKey = rsaKeyFactory.generatePublic(new X509EncodedKeySpec(bytes))
    val cipher = Cipher.getInstance("RSA")
    val public_key_object = rsaPublicKey
    cipher.init(Cipher.ENCRYPT_MODE, public_key_object)
    val session_token = existingToken.getOrElse(generate_random_session_token())
    val encrypted_session_token_bytes =
      cipher.doFinal(Base64.getDecoder.decode(Base64.getEncoder.encode(session_token.getBytes())))
    val encodedMessage = Base64.getEncoder().encodeToString(encrypted_session_token_bytes)
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
    (0 to 128).foldLeft("") { (a, c) =>
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

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    Server.start(8090, app2).exitCode

  case class Player(pubKey: PubKey)

  object Player {
    implicit val decoder: JsonDecoder[Player] = DeriveJsonDecoder.gen[Player]
    implicit val encoder: JsonEncoder[Player] = DeriveJsonEncoder.gen[Player]
    def apply(pukey: PubKey): Player = Player(pukey)
  }

}
