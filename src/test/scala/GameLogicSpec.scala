import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GameLogicSpec extends AnyFunSuite with Matchers {
  private val player = Player(currentLvl = 1, maxEnergy = 100, energy = 50)

  test("вхід у рейд списує 20 енергії та встановлює inRaid") {
    GameLogic.enterRaid(player) shouldBe Right(player.copy(energy = 30, inRaid = true))
  }

  test("вхід у рейд дозволений рівно з 20 одиницями енергії") {
    val ready = player.copy(energy = 20)
    GameLogic.enterRaid(ready) shouldBe Right(ready.copy(energy = 0, inRaid = true))
  }

  test("вхід у рейд заборонений, якщо енергії менше за 20") {
    GameLogic.enterRaid(player.copy(energy = 19)).isLeft shouldBe true
  }

  test("не можна вдруге зайти в рейд, поки перший не завершено") {
    GameLogic.enterRaid(player.copy(inRaid = true)).isLeft shouldBe true
  }

  test("завершення рейду збільшує лічильник і знімає inRaid без додаткових витрат") {
    val inRaid = player.copy(energy = 30, inRaid = true, completedRaids = 2)
    GameLogic.completeRaid(inRaid) shouldBe Right(inRaid.copy(completedRaids = 3, inRaid = false))
  }

  test("не можна завершити рейд, у який не входили") {
    GameLogic.completeRaid(player).isLeft shouldBe true
  }

  test("зілля відновлює рівно 50 одиниць, якщо є місце") {
    GameLogic.usePotion(player.copy(energy = 20)) shouldBe Right(player.copy(energy = 70))
  }

  test("зілля не перевищує максимальний запас енергії") {
    GameLogic.usePotion(player.copy(energy = 80)) shouldBe Right(player.copy(energy = 100))
  }

  test("природне відновлення додає по 1 одиниці на хвилину") {
    GameLogic.regenerateEnergy(player, 30) shouldBe Right(player.copy(energy = 80))
  }

  test("природне відновлення не перевищує максимум навіть за дві години") {
    GameLogic.regenerateEnergy(player.copy(energy = 20), 120) shouldBe Right(player.copy(energy = 100))
  }

  test("відновлення за максимальної енергії - успішна операція без змін") {
    val full = player.copy(energy = 100)
    GameLogic.regenerateEnergy(full, 10) shouldBe Right(full)
  }

  test("нуль хвилин не змінює стан") {
    GameLogic.regenerateEnergy(player, 0) shouldBe Right(player)
  }

  test("від'ємна кількість хвилин повертає Left") {
    GameLogic.regenerateEnergy(player, -1).isLeft shouldBe true
  }

  test("підвищення рівня збільшує рівень і максимальну енергію на 2") {
    GameLogic.lvlUp(player) shouldBe Right(player.copy(currentLvl = 2, maxEnergy = 102))
  }

  test("час до повного відновлення відповідає різниці енергії") {
    GameLogic.timeToFullEnergy(player) shouldBe 50
    GameLogic.timeToFullEnergy(player.copy(energy = 100)) shouldBe 0
  }

  test("аналітика повертає наявну енергію і кількість завершених рейдів") {
    GameLogic.availableEnergy(player) shouldBe 50
    GameLogic.completedRaidsCount(player.copy(completedRaids = 3)) shouldBe 3
  }

  test("усі події послідовного сценарію дають очікуваний фінальний стан") {
    val start = Player(currentLvl = 1, maxEnergy = 100, energy = 60)
    val events = List(EnterRaid, CompleteRaid, TimePassed(25), UsePotion, LevelUp, EnterRaid, CompleteRaid)
    GameLogic.processEvents(start, events) shouldBe Right(
      Player(currentLvl = 2, maxEnergy = 102, energy = 80, completedRaids = 2, inRaid = false)
    )
  }

  test("обробка списку подій зупиняється при забороненому вході в рейд") {
    val start = player.copy(energy = 10)
    GameLogic.processEvents(start, List(EnterRaid, UsePotion)).isLeft shouldBe true
  }

  test("порожній список подій залишає стан незмінним") {
    GameLogic.processEvents(player, Nil) shouldBe Right(player)
  }

  test("некоректний початковий стан відхиляється") {
    GameLogic.processEvents(player.copy(energy = 101), List(UsePotion)).isLeft shouldBe true
    GameLogic.validatePlayer(player.copy(currentLvl = 0)).isLeft shouldBe true
    GameLogic.validatePlayer(player.copy(completedRaids = -1)).isLeft shouldBe true
  }

  test("додавання енергії не переповнює Int") {
    val huge = Player(currentLvl = 1, maxEnergy = Int.MaxValue, energy = Int.MaxValue - 1)
    GameLogic.usePotion(huge) shouldBe Right(huge.copy(energy = Int.MaxValue))
    GameLogic.regenerateEnergy(huge, Int.MaxValue) shouldBe Right(huge.copy(energy = Int.MaxValue))
  }

  test("підвищення рівня не допускає переповнення Int") {
    val nearMax = Player(currentLvl = 1, maxEnergy = Int.MaxValue, energy = 10)
    GameLogic.lvlUp(nearMax).isLeft shouldBe true
  }

  test("лічильник рейдів не переповнюється") {
    val many = player.copy(inRaid = true, completedRaids = Int.MaxValue)
    GameLogic.completeRaid(many).isLeft shouldBe true
  }

  test("виклик processEvent обробляє TimePassed з параметром") {
    GameLogic.processEvent(player, TimePassed(5)) shouldBe Right(player.copy(energy = 55))
  }
}
