package src.com.db

trait ActionResponse {
  val action:Action
}

case class Success(action:Action) extends ActionResponse
case class Failure(action: Action) extends ActionResponse