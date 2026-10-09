case class Player(
    currentLvl: Int,
    maxEnergy: Int,
    energy: Int,
    completedRaids: Int = 0,
    inRaid: Boolean = false
)

sealed trait GameEvent
case object EnterRaid extends GameEvent
case object CompleteRaid extends GameEvent
case object UsePotion extends GameEvent
case object LevelUp extends GameEvent
case class TimePassed(minutes: Int) extends GameEvent
