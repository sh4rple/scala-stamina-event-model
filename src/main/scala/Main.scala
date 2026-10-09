object Main {
  def main(args: Array[String]): Unit = {
    // Сценарій: старт -> рейд -> відновлення -> бонус -> новий рівень -> рейд -> підсумок.
    val initialPlayer = Player(
      currentLvl = 1,
      maxEnergy = 100,
      energy = 60
    )

    val events: List[GameEvent] = List(
      EnterRaid,
      CompleteRaid,
      TimePassed(25),
      UsePotion,
      LevelUp,
      EnterRaid,
      CompleteRaid
    )

    println("=== Варіант 17. Баланс енергії персонажа ===")
    println(s"Початковий стан: $initialPlayer")
    println("Події:")
    events.zipWithIndex.foreach { case (event, index) =>
      println(s"  ${index + 1}. ${GameLogic.describeEvent(event)}")
    }

    GameLogic.processEvents(initialPlayer, events) match {
      case Right(finalPlayer) =>
        println("\n=== Підсумковий стан ===")
        println(s"Рівень: ${finalPlayer.currentLvl}")
        println(s"Доступна енергія: ${GameLogic.availableEnergy(finalPlayer)} / ${finalPlayer.maxEnergy}")
        println(s"Час до повного відновлення: ${GameLogic.timeToFullEnergy(finalPlayer)} хв.")
        println(s"Успішно завершено рейдів: ${GameLogic.completedRaidsCount(finalPlayer)}")
        println(s"Перебуває у рейді: ${finalPlayer.inRaid}")

      case Left(error) =>
        println(s"Помилка під час обробки сценарію: $error")
    }

    //недостатньо енергії для входу в рейд.
    println("\n=== Перевірка забороненої операції ===")
    val exhaustedPlayer = Player(currentLvl = 1, maxEnergy = 100, energy = 10)
    GameLogic.processEvent(exhaustedPlayer, EnterRaid) match {
      case Left(error) => println(s"Очікувана відмова: $error")
      case Right(_)    => println("Помилка перевірки: рейд не мав бути дозволений!")
    }
  }
}
