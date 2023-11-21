package controller

//import controller.auth.GET_ALL_GLOBZ_AUTH
//import controller.auth.SUBSCRIBE_AUTH
import controller.auth._
import entity.WorldBlock
import network.Auth
import src.com.main.scala.entity.Globz
import zio.ZIO
import zio.prelude.Validation

package object auth {
  //authorization layer for each command
  //compose these through flatmap/map/for-comprehension
  //to create a live validation service
  //NOTE: In general where we pass in an id field, this will
  //usually represent an id validated earlier in the session
  //likely through some other Auth service
  type AUTH[A, E, SENDER] = A => ZIO[_ >: SENDER, E, Boolean]
  val get_glob_location: AUTH[GET_GLOB_LOCATION, String, String] = {
    case GET_GLOB_LOCATION(id) => ZIO.succeed(true)
    case _                     => ZIO.fail("")
  }
  val set_glob_location: (Set[String], String) => AUTH[SET_GLOB_LOCATION, String, String] = {
    (serverKeys, senderId) =>
      {
        case SET_GLOB_LOCATION(id, _) =>
          if (serverKeys.contains(senderId)) ZIO.succeed(true)
          else ZIO.fail(s"$senderId is not a server admin but tried setting location for $id")
        case _ => ZIO.fail("")
      }
  }
  val r: (String) => AUTH[RELATE_EGGS, String, String] = senderId => {
    case _                       => ZIO.fail("RELATE EGGS AUTH NOT IMPLEMENTED")
    case RELATE_EGGS(_, _, _, _) => ???
  }


//  val all: String => SerializableCommand[Nothing, Any] => Validation[Nothing, Boolean] = senderID =>
//    op =>
//      Validation.validate(
//        get_glob_location(op),
//        set_glob_location(Set(""), senderID),
//        get_all_globs,
//        relate_eggs
//      )
}
object AuthCommandService {
  val all = ???
}
