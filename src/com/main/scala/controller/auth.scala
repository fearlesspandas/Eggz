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
    case cmd                   => ZIO.fail(s"$cmd not relevant to GET_GLOB_LOCATION")
  }
  val set_glob_location: (Set[String]) => AUTH[String] = server_keys => {
    case SET_GLOB_LOCATION(_, _) => ZIO.service[String].map(server_keys.contains(_))
    case cmd                     => ZIO.fail(s"$cmd not relevant to SET_GLOB_LOCATION")
  }
  val relate_eggs: AUTH[String] = {
    case RELATE_EGGS(_, _, globId, _) => ZIO.service[String].map(senderId => globId == senderId)
    case cmd                          => ZIO.fail(s"$cmd not relevant to RELATE_EGGS")
  }

  val get_all_globs: AUTH[String] = {
    case GET_ALL_GLOBS() => ZIO.succeed(true)
    case cmd             => ZIO.fail(s"$cmd not relevant to GET_ALL_GLOBS")
  }

  val subscribe: AUTH[String] => AUTH[String] = masterAuth => {
    case SUBSCRIBE(query) => masterAuth(query)
    case cmd              => ZIO.fail(s"$cmd not relevant to SUBSCRIBE")
  }
}
object AuthCommandService {

  val base: Set[String] => AUTH[String] = (server_keys: Set[String]) =>
    (op: Any) => {
      val set_glob_loc = set_glob_location(server_keys)(op).debug
      val get_glob_loc = get_glob_location(op).debug
      val rel_egg = relate_eggs(op).debug
      val get_globs = get_all_globs(op).debug
      ZIO
        .validateFirstPar(Seq(set_glob_loc, get_glob_loc, rel_egg, get_globs)) { x =>
          x
        }
        .mapError(_ => "failed base validations")
    }
  val all: (Set[String]) => AUTH[String] =
    (server_keys: Set[String]) =>
      (op: Any) => {
        val other_tests = base(server_keys)
        val sub = subscribe(other_tests(_).mapError(_ => ""))
        ZIO
          .validateFirstPar(Seq(other_tests(op), sub(op))) { x =>
            x
          }
          .mapError(_ => "failed group validate")
      }
}
