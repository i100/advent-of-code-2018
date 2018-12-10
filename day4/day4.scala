import scala.io.Source
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter


case class LogMessage(
  timestamp: LocalDateTime,
  message: String
)

def loadLogData(): List[LogMessage] = {
  val pattern = raw"\[(\d{4}-\d{2}-\d{2}\s\d{2}:\d{2})\] (.+)".r
  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  Source.fromFile("./input")
    .getLines
    .toList
    .map { line => 
      val pattern(timestamp, message) = line 
      LogMessage(LocalDateTime.parse(timestamp, formatter), message)
    }
}

def sortLogData(logData: List[LogMessage]): List[LogMessage] = {
  def sortFn(message1: LogMessage, message2: LogMessage) = {
    // ascending
    message1.timestamp.isBefore(message2.timestamp)
  }

  logData.sortWith(sortFn)
}

val logData: List[LogMessage] = sortLogData(loadLogData)

case class GuardStateTransition(
  guardID: Int, 
  timestamp: LocalDateTime, 
  guardState: GuardState // should be an enum but w/ever
)

abstract class GuardState { }
case class GuardOnDuty() extends GuardState
case class GuardAsleep() extends GuardState
case class GuardAwake() extends GuardState

def parseGuardStateTransitions(logData: List[LogMessage]): 
    List[GuardStateTransition] = {
  val newGuardPattern = "Guard #([0-9]+) begins shift".r

  // track the guard ids across states
  def nextStateTransition(rawLogData: List[LogMessage], 
                          procdStateTransitions: List[GuardStateTransition],
                          curGuardId: Int): 
      List[GuardStateTransition] = {
    rawLogData match {
      case Nil => procdStateTransitions
      case head :: tail =>
        val (guardID, transition): (Int, GuardState) = head.message match {
          case newGuardPattern(newGuardID) => (newGuardID.toInt, GuardOnDuty())
          case m if m.contains("falls asleep") => (curGuardId, GuardAsleep())
          case m if m.contains("wakes up") => (curGuardId, GuardAwake())
          case _ => throw new Exception(s"Syntax error: ${head.message}")
        }
        nextStateTransition(
          tail, 
          GuardStateTransition(guardID, head.timestamp, transition) ::
            procdStateTransitions,
          guardID
        )
    }
  }
  nextStateTransition(logData, List(), 0).reverse
}

// solve the first problem
case class SleepingGuard(
  guardID: Int, 
  asleepAt: LocalDateTime, 
  wakesAt: LocalDateTime,
  minutesAsleep: Int
)

def parseSleepingGuards(stateTransitions: List[GuardStateTransition]):
    List[SleepingGuard] = {
  def invalidStateException(state: GuardStateTransition) = { 
    throw new Exception(s"Invalid state: ${state}")
  }

  def nextStateTransition(stateTransitions: List[GuardStateTransition], 
                          procdTransitions: List[SleepingGuard]): 
      List[SleepingGuard] = {
    stateTransitions match {
      case Nil => procdTransitions
      case head :: next :: tail =>
        head.guardState match {
          case GuardAsleep() =>
            next.guardState match {
              case GuardAwake() =>  
              // GuardAsleep should always follow 
              case _ => invalidStateException(head)
            }
            // can only happen beween 12 and 1, so we only need to worry about 
            // the minutes
            nextStateTransition(
              tail, 
              SleepingGuard(
                head.guardID,
                head.timestamp, 
                next.timestamp, 
                next.timestamp.getMinute - head.timestamp.getMinute
              ) :: procdTransitions
            )
          case GuardAwake() => invalidStateException(head)
          case GuardOnDuty() => 
            nextStateTransition( next :: tail, procdTransitions)
          case _ => invalidStateException(head)
        }
      // should always end with GuardAwake
      case head :: tail => invalidStateException(head)
    }
  }
  nextStateTransition(stateTransitions, List())
}

def sumGuardsAsleep(sleepingGuards: List[SleepingGuard])/*: Map[Int, Int]*/ = {
  sleepingGuards 
    .groupBy(_.guardID)
    .mapValues(guardList => guardList.map(guard => guard.minutesAsleep).sum)
}

def findSnooziestGuard(sleepingGuards: Map[Int, Int]) = { 
  sleepingGuards.maxBy(_._2) 
}

def findSnooziestMinute(guardID: Int, sleepingGuards: List[SleepingGuard]) = {
  val sleepingMinutes = sleepingGuards
    .filter { guard => guard.guardID == guardID }
    // again, only need to be concerned with the minutes
    .flatMap { 
      guard => (guard.asleepAt.getMinute to guard.wakesAt.getMinute - 1).toList
    }
  sleepingMinutes.groupBy(identity).mapValues(_.size).maxBy(_._2)
}

// part 2
def findSnooziestGuardMinute(sleepingGuards: List[SleepingGuard]) = {
  val sleepingGuardMinutes = sleepingGuards 
    .flatMap {
      guard => 
        (guard.asleepAt.getMinute to guard.wakesAt.getMinute - 1)
          .toList.map { m =>
            (guard.guardID, m)
        }
    }
  sleepingGuardMinutes.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
}


val stateTransitions = parseGuardStateTransitions(logData)
val sleepingGuards = parseSleepingGuards(stateTransitions)
val timeAsleep = sumGuardsAsleep(sleepingGuards)
val snooziestGuard = findSnooziestGuard(timeAsleep)
val snooziestMinute = findSnooziestMinute(snooziestGuard._1, sleepingGuards)
val snooziestGuardMinute = findSnooziestGuardMinute(sleepingGuards)

//println(s"Transitions: $stateTransitions")
//println(s"Sleeping Guards: ${parseSleepingGuards(stateTransitions)}")
//println(s"# minutes each guard was asleep: ${timeAsleep}")

println("................")
println(s"Guard most often asleep: ${snooziestGuard._1}, asleep for ${snooziestGuard._2} minutes in total")
println(s"This guard was mostly asleep during minute: ${snooziestMinute._1}, seen ${snooziestMinute._2} times.")
println("Product of guard ID and the minute that guard was most often asleep: ")
println(snooziestGuard._1 * snooziestMinute._1)
println("................")
println(s"The guard/minute that was most often seen asleep: ${snooziestGuardMinute}")
println(s"The product of these two: ")
println(snooziestGuardMinute._1 * snooziestGuardMinute._2)