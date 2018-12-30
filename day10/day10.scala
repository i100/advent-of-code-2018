import scala.io.Source
import scala.io.StdIn


case class Point(x: Int, y: Int, xVelocity: Int, yVelocity: Int)

val inputPattern = raw"^.{10} ?(-?\d{5}),\s*(-?\d{5}).{12} ?(-?\d),\s*(-?\d).+".r
val inputPoints = Source.fromFile("./day10/input")
  .getLines
  .toList
  .map { line =>
    val inputPattern(x, y, xVelocity, yVelocity) = line 
    Point(x.toInt, y.toInt, xVelocity.toInt, yVelocity.toInt)
  }



object Simulation {
  val XThreshold = 80
  val YThreshold = 20

  def printGrid(points: List[Point]): Unit = {
    val xVals = points.map(_.x)
    val yVals = points.map(_.y)

    def expandRangeToThreshold(initialRange: Range, 
        threshold: Int): List[Int] = {
      val thresholdDelta = threshold - (initialRange.max - initialRange.min)
      if (thresholdDelta > 0) {
        (initialRange.min - (thresholdDelta / 2) to 
          initialRange.max + (thresholdDelta / 2))
          .toList
      } else {
        initialRange.toList
      }
    }

    val yRange = expandRangeToThreshold((yVals.min to yVals.max), YThreshold)
    val xRange = expandRangeToThreshold((xVals.min to xVals.max), XThreshold)
    val xMin = xRange.min
    // println(s"Printing grid...\nxVals: $xVals\nyVals: $yVals\n y range: $yRange")

    def makeLine(xRemaining: List[Int],
        procdLine: List[Char]): String = {
      val curXPosition = xMin + procdLine.length

      //println(s"Making line\ncur X: $curXPosition\npoints remaining: $pointsRemaining\nprocd line: $procdLine")

      xRemaining match {
        case l if procdLine.length == XThreshold => 
          procdLine.reverse.mkString
        case Nil => 
          procdLine.reverse.mkString + 
            ("." * (XThreshold - procdLine.length))
        case head :: tail =>
          if (curXPosition == head) {
            makeLine(tail, '#' :: procdLine)
          } else {
            makeLine(xRemaining, '.' :: procdLine)
          }
      }
    }

    for (lineNum <- yRange) {
      val linePoints = points
        .filter { point => (point.y == lineNum) }
        .map(_.x)
        .distinct
        .sortWith(_ < _)

      println(makeLine(linePoints, List()))
    }
  }

  def pointsInProximity(points: List[Point]): Boolean = {
    val xVals = points.map(_.x)
    val yVals = points.map(_.y)
    //println(s"Tick x range: ${xVals.max - xVals.min}, y range: ${yVals.max - yVals.min}")
    xVals.max - xVals.min <= XThreshold &&
      yVals.max - yVals.min <= YThreshold
  }

  def movePoint(point: Point): Point = {
    Point(point.x + point.xVelocity, point.y + point.yVelocity,
      point.xVelocity, point.yVelocity)
  }

  def tick(points: List[Point]): List[Point] = { points.map(movePoint(_)) }

  def handlePointsInProximity(points: List[Point], tickCount: Int) = {
    println(s"Printing tick #${tickCount}")
    printGrid(points)
    StdIn.readLine("Press any button to continue...")
  }

  def run(initialPoints: List[Point]) = {
    var pointsHaveConverged = false 
    def doRun(lastTick: List[Point], tickCount: Int): (List[Point], Int) = {
      if (tickCount % 1000 == 0) println(s"At tick #$tickCount")
      
      val convergence = pointsInProximity(lastTick)
      if (convergence) {
        pointsHaveConverged = true  
        handlePointsInProximity(lastTick, tickCount)
      } 
      if (! convergence && pointsHaveConverged) {
        println(s"Simulation completed in $tickCount ticks")
        (lastTick, tickCount)
      } else {
        doRun(tick(lastTick), tickCount + 1)
      }
    }

    doRun(initialPoints, 0)
  }
}

println(s"Input points: $inputPoints")
Simulation.run(inputPoints)