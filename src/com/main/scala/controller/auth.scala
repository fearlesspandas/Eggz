package controller

//import controller.auth.GET_ALL_GLOBZ_AUTH
//import controller.auth.SUBSCRIBE_AUTH
import controller.auth._
import entity.WorldBlock
import network.Auth
import src.com.main.scala.entity.Globz
import zio.Tag
import zio.ZIO
import zio.prelude.Validation

package object auth {
  //authorization layer for each command
  //compose these through flatmap/map/for-comprehension
  //to create a live validation service
  //NOTE: In general where we pass in an id field, this will
  //usually represent an id validated earlier in the session
  //likely through some other Auth service
  type AUTH[SENDER] = Any => ZIO[SENDER, String, Boolean]

  val get_glob_location: AUTH[String] = {
    case GET_GLOB_LOCATION(id) => ZIO.succeed(true)
    case _                     => ZIO.fail(???)
  }
  val set_glob_location: (Set[String]) => AUTH[String] = server_keys => {
    case SET_GLOB_LOCATION(_, _) => ZIO.service[String].map(server_keys.contains(_))
    case _                       => ZIO.fail(???)
  }
  val relate_eggs: AUTH[String] = ???

  val get_all_globs: AUTH[String] = ???

  val subscribe: AUTH[String] => AUTH[String] = ???
}
object AuthCommandService {
  val all = ???
}
