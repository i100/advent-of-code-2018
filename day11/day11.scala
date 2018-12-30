val SerialNumber = 2568


def calcPowerLevel(x: Int, y: Int) = {
  val s1 = x + 10
  val s2 = s1 * y 
  val s3 = s2 + SerialNumber 
  val s4 = s3 * s1
  val s5 = if (s4 < 100) 0 else (s4 / 100) % (((s4 / 100) / 10) * 10)
  s5 - 5
}

case class Cell(x: Int, y: Int) { val powerLevel: Int = calcPowerLevel(x, y) }

// length = 300^2 = 90000,
// each group of 300 is a row, adjacent y entries are 300 apart
object Grid {
  val gridSize = 300
  val xRange = (1 to gridSize).toList
  val yRange = xRange
  val cells: Array[Cell] = {
    for (y <- yRange; x <- xRange) yield Cell(x, y)
  }.toArray

  def getCell(x: Int, y: Int) = {
    cells((y - 1) * gridSize + (x - 1))
  }

  def getSubGridFromCell(topLeft: Cell, subGridSize: Int): List[Cell] = {
    val subGridRange = (0 to subGridSize - 1).toList
    for(x <- subGridRange; y <- subGridRange)
      yield getCell(topLeft.x + x, topLeft.y + y)
  }

  // return a list of (cell, sub-grid cells)
  def convolve(subGridSize: Int): List[(Cell, List[Cell])] = {
    cells.toList
      .filter { cell =>
        (cell.x <= xRange.max - subGridSize) &&
          (cell.y <= yRange.max - subGridSize)
      }
      .map{ cell => (cell, getSubGridFromCell(cell, subGridSize)) }
  }
}

val DoPart1 = true
val DoPart2 = false

if (DoPart1) {
  val subGrids = Grid.convolve(3)
  val maxSubGrid = subGrids.maxBy { subGrid =>
    subGrid._2.map(_.powerLevel).sum
  }
  val maxPowerLevel = maxSubGrid._2.map(_.powerLevel).sum

  println(s"The top-left cell of the sub-grid with the highest power is:")
  println(s"${maxSubGrid._1}, it has power level $maxPowerLevel.")
}

if (DoPart2) {

}