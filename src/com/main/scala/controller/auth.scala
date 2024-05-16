package controller

import controller.auth._
import zio.ZIO

package object auth {

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
  val get_all_terrain: AUTH[String] = {
    case GET_ALL_TERRAIN() =>
      for {
        send <- ZIO.service[String]
      } yield true
    case cmd => ZIO.fail(s"$cmd not relevant for GET_ALL_TERRAIN")
  }
  val add_terrain: AUTH[String] = {
    case ADD_TERRAIN(id, location) =>
      for {
        sender <- ZIO.service[String]
      } yield true
    case cmd => ZIO.fail(s"$cmd not relevant for ADD_TERRAIN")
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
            get_all_terrain(op),
            add_terrain(op)
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
            get_all_terrain(op),
            add_terrain(op)
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
