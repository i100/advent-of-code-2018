case class Cell(x: Int, y: Int, powerLevel: Int)

// length = 300^2 = 90000,
// each group of 300 is a row, adjacent y entries are 300 apart
class Grid(val gridSize: Int, val serialNumber: Int) {
  val xRange = (1 to gridSize).toList
  val yRange = xRange

  def calcPowerLevel(x: Int, y: Int) = {
    val s1 = x + 10
    val s2 = s1 * y
    val s3 = s2 + serialNumber
    val s4 = s3 * s1
    val s5 = (s4 / 100) % 10
    s5 - 5
  }

  lazy val cells: Array[Cell] = {
    for (y <- yRange; x <- xRange) yield Cell(x, y, calcPowerLevel(x, y))
  }.toArray

  def getCell(x: Int, y: Int) = { cells((y - 1) * gridSize + (x - 1)) }

  def getCellXRange(xMin:Int, xMax: Int, y: Int) = {
    val yBase = (y - 1) * gridSize
    cells.slice(yBase + xMin - 1, yBase + xMax)
  }

  def getSubGridFromCell2(topLeft: Cell, subGridSize: Int): List[Cell] = {
    val subGridRange = (0 to subGridSize - 1).toList
    val xRange = (topLeft.x to topLeft.x + subGridSize)
    val xSlices = for (y <- subGridRange)
      yield getCellXRange(xRange.min, xRange.max, y)
    xSlices.flatten
  }

  def getSubGridFromCell(topLeft: Cell, subGridSize: Int): List[Cell] = {
    val subGridRange = (0 to subGridSize - 1).toList
    for(x <- subGridRange; y <- subGridRange)
      yield getCell(topLeft.x + x, topLeft.y + y)
  }

  def filterTopLeftCells(subGridSize: Int) = {
    cells.toList
      .filter { cell =>
        (cell.x <= xRange.max - subGridSize) &&
          (cell.y <= yRange.max - subGridSize) }
  }

  def convolve(subGridSize: Int, applyToSubGrid: List[Cell] => Any = identity):
      List[(Cell, Any)] = {
    filterTopLeftCells(subGridSize)
      .map { cell =>
        (cell, applyToSubGrid(getSubGridFromCell(cell, subGridSize))) }
  }

  def maxPowerConvolve(subGridSize: Int): (Cell, Int) = {
      // applyAggregate: List[(Cell, Any)] => Any,
      // applyToSubGrid: List[Cell] => Any = identity): Any = {
    filterTopLeftCells(subGridSize)
      .foldLeft((Cell(0, 0, 0), -999)) { (acc, cell) =>
        val subGridSum = getSubGridFromCell(cell, subGridSize)
          .foldLeft(0) { (acc, cell) => acc + cell.powerLevel }

        if (subGridSum > acc._2) (cell, subGridSum) else acc
      }
  }

  def maxPowerConvolveAll(): (Cell, Int, Int) = {
    (2 to gridSize).toList
      .map { n =>
        println(s"Processing grid size $n")
        val thisMax = maxPowerConvolve(n)
        (thisMax._1, thisMax._2, n) }
      .maxBy(_._2)
  }
}


val DoPart1 = true
val DoPart2 = true
val SerialNumber = 2568

val grid = new Grid(300, SerialNumber)

def sumSubGrid(subGrid: List[Cell]) = { subGrid.map(_.powerLevel).sum }

if (DoPart1) {
  val subGrids = grid.convolve(3, sumSubGrid)
  val maxSubGrid = subGrids.maxBy(_._2.toString.toInt)
  val topLeftCell = maxSubGrid._1

  println(s"Part 1 answer: ${topLeftCell.x}, ${topLeftCell.y}")
}

if (DoPart2) {
  val maxSubGrid = grid.maxPowerConvolveAll()
  val topLeftCell = maxSubGrid._1
  val numConvolutions = maxSubGrid._3

  println(s"Part 2 answer: ${topLeftCell.x}, ${topLeftCell.y}, " +
    s"$numConvolutions")
}
