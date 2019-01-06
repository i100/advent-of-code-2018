import scala.io.Source


case class Pot(id: Int, isFull: Boolean) {
  val symbl = if (isFull) '#' else '.'
}

val input = Source.fromFile("./day12/input")
  .getLines
  .toList

val initialState = input.head
  .toList
  .filter { c => c == '.' || c == '#' }
  .zipWithIndex
  .map(e => Pot(e._2, if (e._1 == '#') true else false))

val evolutionRules: Map[String, Char] = input.drop(2)
  .map(_.split(" => "))
  .map(a => (a(0), a(1)(0))).toMap

def score(state: List[Pot]) = {
  state.foldLeft(0) { (acc, e) =>
    if (e.isFull) acc + e.id else acc
  }
}

def padState(state: List[Pot]): List[Pot] = {
  // pad the input on both ends so that there are at least 5 .'s
  def countEmptyPotsOnLeft(remaining: List[Pot], seen: Int = 0): Int = {
    remaining match {
      case Nil => seen
      case head :: tail =>
        if (! head.isFull) countEmptyPotsOnLeft(tail, seen + 1) else seen
    }
  }
  val addLeft: Int = Seq(5 - countEmptyPotsOnLeft(state), 0).max
  val addRight: Int = Seq(5 - countEmptyPotsOnLeft(state.reverse), 0).max
  val l = if (addLeft > 0) {
    (state.head.id - 1 to state.head.id - addLeft by -1).toList.map { i =>
      Pot(i, isFull = false) }.reverse
  } else {
    Nil
  }
  val r = if (addRight > 0) {
    (state.last.id + 1 to state.last.id + addRight).toList.map { i =>
      Pot(i, isFull = false) }
  } else {
    Nil
  }

  l ::: state ::: r
}

def evolve(state: List[Pot]): List[Pot] = {
  val paddedState: List[Pot] = padState(state)
  def matchPattern(centredAtID: Int)= {
    val centredAtIndex = paddedState.map(_.id)indexOf(centredAtID)
    val rawPattern = paddedState
      .slice(centredAtIndex - 2, centredAtIndex + 3)
      .map(_.symbl)
      .mkString

    evolutionRules(rawPattern)
  }

  val nextState = paddedState
    .map { p =>
      if (p.id > paddedState.head.id + 1 && p.id < paddedState.last.id - 1) {
        matchPattern(p.id)
      } else {
        p
      }
    }
    .zip(paddedState.map(_.id))
    .map { e => Pot(e._2, if (e._1 == '#') true else false) }

  val stateScore = score(nextState)

  println(s"State score for this evolution: $stateScore")

  nextState
}

def simulate(initialState: List[Pot], evolutions: Int): List[Pot] = {
  if (evolutions % 1000 == 0) println(s"Evolutions remaining: $evolutions")
  if (evolutions <= 0) {
    initialState
  } else {
    simulate(evolve(initialState), evolutions - 1)
  }
}


val evolutions = 10000

val results = simulate(initialState, evolutions)
println(s"Simulation results: $results")
