object GameLogic {
  val RaidCost: Int = 20
  val PotionEnergy: Int = 50
  val EnergyPerMinute: Int = 1
  val LevelEnergyBonus: Int = 2

  def validatePlayer(player: Player): Either[String, Player] = {
    if (player.currentLvl < 1) {
      Left("Рівень персонажа має бути не меншим за 1.")
    } else if (player.maxEnergy <= 0) {
      Left("Максимальна енергія має бути додатною.")
    } else if (player.energy < 0 || player.energy > player.maxEnergy) {
      Left("Поточна енергія має бути від 0 до максимальної енергії.")
    } else if (player.completedRaids < 0) {
      Left("Кількість завершених рейдів не може бути від'ємною.")
    } else {
      Right(player)
    }
  }

  def lvlUp(player: Player): Either[String, Player] = {
    validatePlayer(player).flatMap { validPlayer =>
      if (validPlayer.currentLvl == Int.MaxValue ||
          validPlayer.maxEnergy > Int.MaxValue - LevelEnergyBonus) {
        Left("Неможливо підвищити рівень: перевищено допустимі числові межі.")
      } else {
        Right(validPlayer.copy(
          currentLvl = validPlayer.currentLvl + 1,
          maxEnergy = validPlayer.maxEnergy + LevelEnergyBonus
        ))
      }
    }
  }

  def usePotion(player: Player): Either[String, Player] = {
    validatePlayer(player).map { validPlayer =>
      val restoredEnergy = validPlayer.energy.toLong + PotionEnergy
      validPlayer.copy(energy = math.min(restoredEnergy, validPlayer.maxEnergy.toLong).toInt)
    }
  }

  def regenerateEnergy(player: Player, minutes: Int): Either[String, Player] = {
    validatePlayer(player).flatMap { validPlayer =>
      if (minutes < 0) {
        Left("Кількість хвилин не може бути від'ємною.")
      } else {
        val gainedEnergy = minutes.toLong * EnergyPerMinute
        val restoredEnergy = validPlayer.energy.toLong + gainedEnergy
        Right(validPlayer.copy(
          energy = math.min(restoredEnergy, validPlayer.maxEnergy.toLong).toInt
        ))
      }
    }
  }

  def enterRaid(player: Player): Either[String, Player] = {
    validatePlayer(player).flatMap { validPlayer =>
      if (validPlayer.inRaid) {
        Left("Гравець уже перебуває в рейді.")
      } else if (validPlayer.energy < RaidCost) {
        val waitingMinutes = RaidCost - validPlayer.energy
        Left(s"Недостатньо енергії для рейду. Потрібно зачекати $waitingMinutes хвилин або використати зілля.")
      } else {
        Right(validPlayer.copy(
          energy = validPlayer.energy - RaidCost,
          inRaid = true
        ))
      }
    }
  }

  def completeRaid(player: Player): Either[String, Player] = {
    validatePlayer(player).flatMap { validPlayer =>
      if (!validPlayer.inRaid) {
        Left("Гравець не перебуває в рейді.")
      } else if (validPlayer.completedRaids == Int.MaxValue) {
        Left("Лічильник завершених рейдів досяг максимально допустимого значення.")
      } else {
        Right(validPlayer.copy(
          completedRaids = validPlayer.completedRaids + 1,
          inRaid = false
        ))
      }
    }
  }

  def timeToFullEnergy(player: Player): Int =
    (player.maxEnergy - player.energy) / EnergyPerMinute

  def availableEnergy(player: Player): Int = player.energy

  def completedRaidsCount(player: Player): Int = player.completedRaids

  def processEvent(player: Player, event: GameEvent): Either[String, Player] = {
    event match {
      case EnterRaid           => enterRaid(player)
      case CompleteRaid        => completeRaid(player)
      case UsePotion           => usePotion(player)
      case LevelUp             => lvlUp(player)
      case TimePassed(minutes) => regenerateEnergy(player, minutes)
    }
  }

  def processEvents(initialPlayer: Player, events: List[GameEvent]): Either[String, Player] = {
    events.foldLeft[Either[String, Player]](validatePlayer(initialPlayer)) {
      (state, event) =>
        state.flatMap(currentPlayer => processEvent(currentPlayer, event))
    }
  }

  def describeEvent(event: GameEvent): String = event match {
    case EnterRaid           => "Вхід у рейд"
    case CompleteRaid        => "Успішне завершення рейду"
    case UsePotion           => "Використання зілля"
    case LevelUp             => "Підвищення рівня"
    case TimePassed(minutes) => s"Минуло $minutes хв."
  }
}
