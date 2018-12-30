import scala.collection.mutable.{ ArrayBuffer, ListBuffer }
import scala.math.abs
import scala.annotation.tailrec


case class GameResult(ring: List[Int], scores: List[Int])

class Game(numPlayers: Int, numTurns: Int) {
  val scores = ArrayBuffer.fill(NumPlayers)(0)
  val ring = ListBuffer(0)
  var curIndex = 0
  var turnNumber = 0

  @tailrec
  private def nextTurn(): GameResult = {
    turnNumber += 1
    if (turnNumber % 5000 == 0) println(s"At turn #$turnNumber")
    if (turnNumber < 28) println(
      s"Turn #${"%02d".format(turnNumber - 1)}: ${ring}")
    if (turnNumber % 23 == 0) {
      val removeIndex = abs((curIndex - 7) % ring.length)
      scores(turnNumber % numPlayers) +=
        (turnNumber + ring(removeIndex))
      ring.remove(removeIndex)
      curIndex = removeIndex
    } else {
      val insertIndex = abs((curIndex + 1) % ring.length) + 1
      ring.insert(insertIndex, turnNumber)
      curIndex = insertIndex
    }

    if (turnNumber == numTurns) {
      GameResult(ring.toList, scores.toList)
    } else {
      nextTurn()
    }
  }

  def run = {
    println("Running game...")
    nextTurn()
  }
}


val NumPlayers = 464
val NumTurnsPart1 = 71730
val NumTurnsPart2 = NumTurnsPart1 * 100

val DoPart1 = false
val DoPart2 = true

if (DoPart1) {
  val game = new Game(NumPlayers, NumTurnsPart1)
  val finalTurn = game.run
  println(s"Highest score (part 1): ${finalTurn.scores.max}")
}

if (DoPart2) {
  val game = new Game(NumPlayers, NumTurnsPart2)
  val finalTurn = game.run
  println(s"Highest score (part 2) : ${finalTurn.scores.max}")
}