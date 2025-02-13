package entity

import entity.Ability.ABILITY_ID
import zio.*

object Pocket {
  type POCKET_STORE = Ref[Map[ABILITY_ID, Int]]
  def make: UIO[Ref[Map[ABILITY_ID, Int]]] =
    Ref.make(Map.empty[ABILITY_ID, Int])

  trait Service {
    val pocket_contents: Ref[Map[ABILITY_ID, Int]]

    def pocketAbility(id: ABILITY_ID, amount: Int = 1): UIO[Unit] =
      this.pocket_contents.update(m =>
        m.updated(id, m.getOrElse(id, 0) + amount)
      )

    def removePocketAbility(
      id: ABILITY_ID,
      amount: Int = 1
    ): UIO[Unit] =
      this.pocket_contents.update(m =>
        m.updated(id, scala.math.max(m.getOrElse(id, 0) - amount, 0))
      )

  }

  trait PocketError {}

}
