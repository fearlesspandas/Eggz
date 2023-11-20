package controller

//import controller.auth.GET_ALL_GLOBZ_AUTH
import controller.auth.GET_GLOB_LOCATION_AUTH
import controller.auth.SET_GLOB_LOCATION_AUTH
//import controller.auth.SUBSCRIBE_AUTH
import entity.WorldBlock
import network.Auth
import network.AuthError
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

  def get_glob_auth(command: Command[Nothing, Any]): Validation[String, Boolean] = command match {
    case GET_GLOB_LOCATION(_) => Validation.succeed(true)
  }
  case class GET_GLOB_LOCATION_AUTH(validId: String) extends Auth[GET_GLOB_LOCATION, String] {
    override def validation[R1 >: GET_GLOB_LOCATION]
      : PartialFunction[R1, Validation[Boolean, Boolean]] = {
      case GET_GLOB_LOCATION(id) => Validation.succeed(true)
    }
  }
  def set_glob_location_auth(server_keys: Set[String], senderId: String)(
    op: Command[Nothing, Any]
  ): Validation[String, Boolean] = op match {
    case SET_GLOB_LOCATION(id, _) => Validation.succeed(server_keys.contains(senderId))
    case _                        => Validation.succeed(false)
  }
  case class SET_GLOB_LOCATION_AUTH(server_keys: Set[String], senderid: String)
      extends Auth[SET_GLOB_LOCATION, String] {
    override def validation[R1 >: SET_GLOB_LOCATION]
      : PartialFunction[R1, Validation[Boolean, Boolean]] = {
      case SET_GLOB_LOCATION(id, _) =>
        Validation.succeed(server_keys.contains(senderid))
    }
  }

//  def get_all_globs_auth(senderId:String)(op:Command[Nothing,Any])
//  //currently turned off until I work out the right protocol for entity creation
//  case class CREATE_GLOB_AUTH() extends Auth[CREATE_GLOB, String, Boolean] {
//    override def validate[S <: String, R1 >: CREATE_GLOB](
//      id: S,
//      op: R1
//    ): ZIO[Any, Nothing, Boolean] = ZIO.succeed(false)
//
//    override def validation[R1 >: CREATE_GLOB]
//      : PartialFunction[R1, ZIO[String, Nothing, Boolean]] = {
//      case _ => ZIO.succeed(false)
//    }
//  }
//
//  case class RELATE_EGGS_AUTH() extends Auth[RELATE_EGGS, String, Boolean] {
//    override def validate[S <: String, R1 >: RELATE_EGGS](
//      id: S,
//      op: R1
//    ): ZIO[Any, Nothing, Boolean] = ???
//
//    override def validation[R1 >: RELATE_EGGS]: PartialFunction[R1, ZIO[String, Nothing, Boolean]] =
//      ???
//  }
//
//  case class SUBSCRIBE_AUTH[B >: ResponseQuery[SUBSCRIBE.SubscriptionEnv]](
//    valid: String,
//    masterAuth: Auth[B, String, Boolean]
//  ) extends Auth[SUBSCRIBE, String, Boolean] {
//    override def validate[S <: String, R1 >: SUBSCRIBE](id: S, op: R1): ZIO[Any, Nothing, Boolean] =
//      op match {
//        case SUBSCRIBE(query) =>
//          query match {
//            case GET_GLOB_LOCATION(qid) => ZIO.succeed(true)
//            case x                      => masterAuth.validate(id, x)
//          }
//        case _ => ZIO.succeed(false)
//      }
//
//    override def validation[R1 >: SUBSCRIBE]: PartialFunction[R1, ZIO[String, Nothing, Boolean]] = {
//      case SUBSCRIBE(query) => ???
//    }
//  }

}
object AuthCommandService {

//  def all(
//    id: String,
//    serverIds: Set[String]
//  ): Auth[SerializableCommand[Globz.Service with WorldBlock.Block, Any], String, Boolean] =
//    for {
//      allglobs <- GET_ALL_GLOBZ_AUTH()
//      loc <- GET_GLOB_LOCATION_AUTH(id)
//      set_loc <- SET_GLOB_LOCATION_AUTH(serverIds)
//    } yield allglobs || loc || set_loc
//  def live(
//    id: String,
//    serverIds: Set[String]
//  ): Auth[SerializableCommand[Globz.Service with WorldBlock.Block, Any], String, Boolean] =
//    for {
//      a <- all(id, serverIds)
//      sub <- SUBSCRIBE_AUTH(id, all(id, serverIds))
//    } yield a || sub
}
