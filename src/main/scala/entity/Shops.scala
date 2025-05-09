package entity

import controller.ItemAdded
import controller.ItemRemoved
import controller.MultiResponse
import controller.PhysStat
import controller.QueryResponse
import controller.QueuedClientMessage
import entity.Ability.ABILITY_ID
import entity.Globz.GLOBZ_ID
import zio.*

object Shops {
  def buy_ability(
    id: GLOBZ_ID,
    ability_id: ABILITY_ID
  ): ZIO[WorldBlock.Block, ShopError, QueryResponse] = for {
    entity <- WorldBlock
      .getBlob(id)
      .mapBoth(
        _ => BuyAbilityError("No entity with that id found"),
        { case li: LivingEntity => li }
      )
    res <- ability_id match {
      case 0 =>
        for {
          _ <- entity
            .adjustMaxSpeed(-100)
            .orElseFail(BuyAbilityError("Error while spending max speed"))
          max_speed <- entity.getMaxSpeed.orElseFail(BuyAbilityError(""))
          speed <- entity.getSpeed.orElseFail(BuyAbilityError(""))
          _ <- entity.add(ability_id).orElseFail(BuyAbilityError(""))
        } yield MultiResponse(
          Chunk(
            QueuedClientMessage(
              id,
              Chunk(
                PhysStat(id, max_speed, speed),
                ItemAdded(id, ability_id)
              )
            )
          )
        )
      case 1 =>
        for {
          _ <- entity
            .adjustMaxSpeed(-1000)
            .orElseFail(BuyAbilityError("Error while spending max speed"))
          max_speed <- entity.getMaxSpeed.orElseFail(BuyAbilityError(""))
          speed <- entity.getSpeed.orElseFail(BuyAbilityError(""))
          _ <- entity.add(ability_id).orElseFail(BuyAbilityError(""))
        } yield MultiResponse(
          Chunk(
            QueuedClientMessage(
              id,
              Chunk(
                PhysStat(id, max_speed, speed),
                ItemAdded(id, ability_id)
              )
            )
          )
        )
      case 2 =>
        for {
          _ <- entity
            .adjustMaxSpeed(-200)
            .orElseFail(BuyAbilityError("Error while spending max speed"))
          max_speed <- entity.getMaxSpeed.orElseFail(BuyAbilityError(""))
          speed <- entity.getSpeed.orElseFail(BuyAbilityError(""))
          _ <- entity.add(ability_id).orElseFail(BuyAbilityError(""))
        } yield MultiResponse(
          Chunk(
            QueuedClientMessage(
              id,
              Chunk(
                PhysStat(id, max_speed, speed),
                ItemAdded(id, ability_id)
              )
            )
          )
        )
      case _ => ZIO.fail(BuyAbilityError("No Ability Found"))
    }
  } yield res
  def sell_ability(
    id: GLOBZ_ID,
    ability_id: ABILITY_ID
  ): ZIO[WorldBlock.Block, ShopError, QueryResponse] = for {
    entity <- WorldBlock
      .getBlob(id)
      .mapBoth(
        _ => BuyAbilityError("No entity with that id found"),
        { case li: LivingEntity => li }
      )
    inventory <- entity.getInventory().orElseFail(BuyAbilityError(""))
    res <- (ability_id match {
      case 0 =>
        for {
          _ <- entity
            .adjustMaxSpeed(10)
            .orElseFail(BuyAbilityError("Error while spending max speed"))
          max_speed <- entity.getMaxSpeed.orElseFail(BuyAbilityError(""))
          speed <- entity.getSpeed.orElseFail(BuyAbilityError(""))
          _ <- entity.remove(ability_id).orElseFail(BuyAbilityError(""))
        } yield MultiResponse(
          Chunk(
            QueuedClientMessage(
              id,
              Chunk(
                PhysStat(id, max_speed, speed),
                ItemRemoved(id, ability_id)
              )
            )
          )
        )
      case 1 =>
        for {
          _ <- entity
            .adjustMaxSpeed(100)
            .orElseFail(BuyAbilityError("Error while spending max speed"))
          max_speed <- entity.getMaxSpeed.orElseFail(BuyAbilityError(""))
          speed <- entity.getSpeed.orElseFail(BuyAbilityError(""))
          _ <- entity.remove(ability_id).orElseFail(BuyAbilityError(""))
        } yield MultiResponse(
          Chunk(
            QueuedClientMessage(
              id,
              Chunk(
                PhysStat(id, max_speed, speed),
                ItemRemoved(id, ability_id)
              )
            )
          )
        )
      case 2 =>
        for {
          _ <- entity
            .adjustMaxSpeed(20)
            .orElseFail(BuyAbilityError("Error while spending max speed"))
          max_speed <- entity.getMaxSpeed.orElseFail(BuyAbilityError(""))
          speed <- entity.getSpeed.orElseFail(BuyAbilityError(""))
          _ <- entity.remove(ability_id).orElseFail(BuyAbilityError(""))
        } yield MultiResponse(
          Chunk(
            QueuedClientMessage(
              id,
              Chunk(
                PhysStat(id, max_speed, speed),
                ItemRemoved(id, ability_id)
              )
            )
          )
        )
      case _ => ZIO.fail(BuyAbilityError("No Ability Found"))
    }).when(inventory.contains(ability_id))
      .flatMap(ZIO.fromOption(_))
      .orElseFail(BuyAbilityError(""))
  } yield res
}
trait ShopError
case class BuyAbilityError(msg: String) extends ShopError
