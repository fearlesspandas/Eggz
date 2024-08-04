package controller

import controller.auth.*
import zio.Ref
import zio.ZIO

package object auth {

  case class CommandAuth(authorizer: AUTH[String]) {}

  // authorization layer for each command
  // compose these through flatmap/map/for-comprehension
  // to create a live validation service.
  // We use ZIO.validate for easy parallelism
  type AUTH[SENDER] = Any => ZIO[SENDER, String, Boolean]

  val get_glob_location: AUTH[String] = {
    case GET_GLOB_LOCATION(id) => ZIO.succeed(true)
    case cmd => ZIO.fail(s"$cmd not relevant to GET_GLOB_LOCATION")
  }

  val set_glob_location: (Set[String]) => AUTH[String] = server_keys => {
    case SET_GLOB_LOCATION(_, _) =>
      ZIO.service[String].map(server_keys.contains(_))
    case cmd => ZIO.fail(s"$cmd not relevant to SET_GLOB_LOCATION")
  }

  val relate_eggs: AUTH[String] = {
    case RELATE_EGGS(_, _, globId, _) =>
      ZIO.service[String].map(senderId => globId == senderId)
    case cmd => ZIO.fail(s"$cmd not relevant to RELATE_EGGS")
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

  val get_next_destination: Set[String] => AUTH[String] = server_keys => {
    case GET_NEXT_DESTINATION(id) =>
      for {
        senderId <- ZIO.service[String]
      } yield server_keys.contains(senderId)
    case cmd => ZIO.fail(s"$cmd not relevant to GET_NEXT_DESTINATION")
  }
  val get_all_destinations: AUTH[String] = {
    case GET_ALL_DESTINATIONS(id) =>
      for {
        senderId <- ZIO.service[String]
      } yield id == senderId
    case cmd => ZIO.fail(s"$cmd not relevant to GET_ALL_DESTINATIONS")
  }
  val apply_vector: AUTH[String] = {
    case APPLY_VECTOR(id, _) =>
      for {
        senderId <- ZIO.service[String]
      } yield id == senderId
    case cmd => ZIO.fail(s"$cmd not relevant to APPLY_VECTOR")
  }
  val get_input_vector: Set[String] => AUTH[String] = server_keys => {
    case GET_INPUT_VECTOR(id) =>
      for {
        senderId <- ZIO.service[String]
      } yield server_keys.contains(senderId)
    case cmd => ZIO.fail(s"$cmd not relevant to GET_INPUT_VECTOR")
  }
  val clear_destinations: AUTH[String] = {
    case CLEAR_DESTINATIONS(id) =>
      for {
        senderId <- ZIO.service[String]
      } yield senderId == id
    case cmd => ZIO.fail(s"$cmd not relevant to CLEAR_DESTINATIONS")
  }
  val set_lv: Set[String] => AUTH[String] = server_keys => {
    case SET_LV(id, _) =>
      for {
        senderId <- ZIO.service[String]
      } yield server_keys.contains(senderId)
    case cmd => ZIO.fail(s"$cmd not relevant for SET_LV")
  }
  val lazy_lv: AUTH[String] = {
    case LAZY_LV(id) =>
      for {
        senderId <- ZIO.service[String]
      } yield senderId == id
    case cmd => ZIO.fail(s"$cmd not relevant for LAZY_LV")
  }
  val adjust_physical_stats: AUTH[String] = {
    case ADJUST_PHYSICAL_STATS(id, _) =>
      for {
        sender <- ZIO.service[String]
      } yield sender == id
    case cmd => ZIO.fail(s"$cmd not relevant for ADJUST_PHYSICAL_STATS")
  }
  val get_physical_stats: Set[String] => AUTH[String] = server_keys => {
    case GET_PHYSICAL_STATS(id) =>
      for {
        sender <- ZIO.service[String]
      } yield server_keys.contains(sender) || sender == id
    case cmd => ZIO.fail(s"$cmd not relevant for GET_PHYSICAL_STATS")
  }
  val get_all_terrain: Set[String] => AUTH[String] = server_keys => {
    case GET_ALL_TERRAIN(id, non_relative) =>
      for {
        send <- ZIO.service[String]
      } yield id == send || (non_relative && server_keys.contains(send))
    case cmd => ZIO.fail(s"$cmd not relevant for GET_ALL_TERRAIN")
  }
  val get_terrain_within_distance: Set[String] => AUTH[String] = server_keys =>
    {
      case GET_TERRAIN_WITHIN_DISTANCE(location, radius) =>
        for {
          sender <- ZIO.service[String]
        } yield server_keys.contains(sender)
      case cmd => ZIO.fail(s"$cmd not relevant for GET_TERRAIN_WITHIN_DISTANCE")
    }

  val get_terrain_within_player_distance: Set[String] => AUTH[String] =
    server_keys => {
      case GET_TERRAIN_WITHIN_PLAYER_DISTANCE(id, radius) =>
        for {
          sender <- ZIO.service[String]
        } yield sender == id || server_keys.contains(sender)
      case cmd =>
        ZIO.fail(s"$cmd not relevant for GET_TERRAIN_WITHIN_PLAYER_DISTANCE")
    }
  val add_terrain: AUTH[String] = {
    case ADD_TERRAIN(id, location) =>
      for {
        sender <- ZIO.service[String]
      } yield true
    case cmd => ZIO.fail(s"$cmd not relevant for ADD_TERRAIN")
  }
  val get_top_level_terrain: AUTH[String] = {
    case GET_TOP_LEVEL_TERRAIN() =>
      for {
        sender <- ZIO.service[String]
      } yield true
    case cmd => ZIO.fail(s"$cmd not relevant for GET_TOP_LEVEL_TERRAIN")
  }
  val get_top_level_terrain_in_distance: AUTH[String] = {
    case GET_TOP_LEVEL_TERRAIN_IN_DISTANCE(_, _) =>
      for {
        sender <- ZIO.service[String]
      } yield true
    case cmd =>
      ZIO.fail(s"$cmd not relevant for GET_TOP_LEVEL_TERRAIN_IN_DISTANCE")
  }
  val get_cached_terrain: AUTH[String] = {
    case GET_CACHED_TERRAIN(_) =>
      for {
        sender <- ZIO.service[String]
      } yield true
    case cmd => ZIO.fail(s"$cmd not relevant for GET_CACHED_TERRAIN")
  }
  val next_cmd: AUTH[String] = {
    case NEXT_CMD() =>
      for {
        sender <- ZIO.service[String]
      } yield true
    case cmd => ZIO.fail(s"$cmd not relevant for NEXT_CMD")
  }
  val subscribe: AUTH[String] => AUTH[String] = masterAuth => {
    case SUBSCRIBE(query) => masterAuth(query)
    case cmd              => ZIO.fail(s"$cmd not relevant to SUBSCRIBE")
  }
  val console: AUTH[String] => AUTH[String] = masterAuth => {
    case CONSOLE(_, query) => masterAuth(query)
    case cmd               => ZIO.fail(s"$cmd not relevant to CONSOLE")
  }
}
object AuthCommandService {

  val base: Set[String] => AUTH[String] = (server_keys: Set[String]) =>
    (op: Any) =>
      ZIO
        .validateFirstPar(
          Seq(
            set_glob_location(server_keys)(op),
            get_glob_location(op),
            relate_eggs(op),
            get_all_globs(op),
            add_destination(op),
            get_next_destination(server_keys)(op),
            get_all_destinations(op),
            apply_vector(op),
            get_input_vector(server_keys)(op),
            clear_destinations(op),
            set_lv(server_keys)(op),
            lazy_lv(op),
            adjust_physical_stats(op),
            get_physical_stats(server_keys)(op),
            get_all_terrain(server_keys)(op),
            get_terrain_within_distance(server_keys)(op),
            get_terrain_within_player_distance(server_keys)(op),
            add_terrain(op),
            get_top_level_terrain(op),
            get_top_level_terrain_in_distance(op),
            get_cached_terrain(op),
            next_cmd(op)
          )
        ) { x =>
          x
        }
        .mapError(_ => "failed base validations")

  val base_non_par: Set[String] => AUTH[String] = (server_keys: Set[String]) =>
    (op: Any) =>
      ZIO
        .validateFirstPar(
          Seq(
            set_glob_location(server_keys)(op),
            get_glob_location(op),
            relate_eggs(op),
            get_all_globs(op),
            add_destination(op),
            get_next_destination(server_keys)(op),
            get_all_destinations(op),
            apply_vector(op),
            get_input_vector(server_keys)(op),
            clear_destinations(op),
            set_lv(server_keys)(op),
            lazy_lv(op),
            adjust_physical_stats(op),
            get_physical_stats(server_keys)(op),
            get_all_terrain(server_keys)(op),
            get_terrain_within_distance(server_keys)(op),
            get_terrain_within_player_distance(server_keys)(op),
            add_terrain(op),
            get_top_level_terrain(op),
            get_top_level_terrain_in_distance(op),
            get_cached_terrain(op),
            next_cmd(op)
          )
        ) { x =>
          x
        }
        .mapError(_ => "failed base validations")

  val all: (Set[String]) => AUTH[String] =
    (server_keys: Set[String]) =>
      (op: Any) =>
        {
          val other_tests = base(server_keys)
          val sub = subscribe(other_tests(_))
          val cnsl = console(other_tests(_))
          ZIO
            .validateFirstPar(Seq(other_tests(op), sub(op), cnsl(op))) { x =>
              x
            }
            .mapError(_ => "failed group validate")
        }.fold(_ => false, x => x)

  val all_non_par: (Set[String]) => AUTH[String] =
    (server_keys: Set[String]) =>
      (op: Any) =>
        {
          val other_tests = base_non_par(server_keys)
          val sub = subscribe(other_tests(_).mapError(_ => ""))
          val cnsl = console(other_tests(_))
          ZIO
            .validateFirstPar(Seq(other_tests(op), sub(op), cnsl(op))) { x =>
              x
            }
            .mapError(_ => "failed group validate")
        }.fold(_ => false, x => x)
}

case class AuthenticationService(
  cachedAuth: Ref[Map[Any, Boolean]],
  authorizer: AUTH[String]
) {
  def verify_with_caching: AUTH[String] = (cmd: Any) =>
    cmd match {
      case command: SerializableCommand[_, _] =>
        for {
          cached <- cachedAuth.get.map(_.get(command.REF_TYPE))
          res <- cached match {
            case Some(r) =>
              ZIO.succeed(r)
            case None =>
              for {
                _ <- ZIO.log("creating cached")
                rres <- authorizer(cmd)
                _ <- cachedAuth.update(_.updated(command.REF_TYPE, rres))
              } yield rres
          }
        } yield res
      case _ => ZIO.succeed(false)
    }
}
