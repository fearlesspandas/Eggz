package controller

import controller.auth._
import zio.ZIO

package object auth {
  //authorization layer for each command
  //compose these through flatmap/map/for-comprehension
  //to create a live validation service.
  //We use ZIO.validate for easy parallelism
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

  val add_destination: AUTH[String] = {
    case ADD_DESTINATION(id, _) =>
      for {
        senderId <- ZIO.service[String]
      } yield senderId == id
    case cmd => ZIO.fail(s"$cmd not relevant to ADD_DESTINATION")
  }

  val get_next_destination: AUTH[String] = {
    case GET_NEXT_DESTINATION(id) =>
      for {
        senderId <- ZIO.service[String]
      } yield id == senderId
    case cmd => ZIO.fail(s"$cmd not relevant to GET_NEXT_DESTINATION")
  }

  val subscribe: AUTH[String] => AUTH[String] = masterAuth => {
    case SUBSCRIBE(query) => masterAuth(query)
    case cmd              => ZIO.fail(s"$cmd not relevant to SUBSCRIBE")
  }
}
object AuthCommandService {

  val base: Set[String] => AUTH[String] = (server_keys: Set[String]) =>
    (op: Any) => {
      ZIO
        .validateFirstPar(
          Seq(
            set_glob_location(server_keys)(op),
            get_glob_location(op),
            relate_eggs(op),
            get_all_globs(op),
            add_destination(op),
            get_next_destination(op)
          )
        ) { x =>
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
