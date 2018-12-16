import scala.io.Source
import scala.math.abs 


abstract class GridPoint() { 
  val x: Int 
  val y: Int
}

case class Coord(x: Int, y: Int, id: Int) extends GridPoint { }
case class ClosestCoords( // part 1
  x: Int, 
  y: Int, 
  manHattanDistance: Int,
  coords: List[Coord]
) extends GridPoint { } 
case class TotalDistance( // part 2
  x: Int, 
  y: Int, 
  distance: Int
)
val LOCALITY = 10000

val inputCoords = Source.fromFile("./input")
  .getLines
  .toList 
  .zipWithIndex
  .map { l => 
    val coordString = l._1 
    val index = l._2
    val xy = coordString.replace(" ", "").split(",").toList.map(_.toInt)
    Coord(x = xy(0), y = xy(1), id = index)
  }

// returns a 4-tuple like (minX, maxX, minY, maxY)
def findAxesExtrema(coords: List[Coord]): (Int, Int, Int, Int) = {
  (coords.minBy(_.x).x - 1, coords.maxBy(_.x).x + 1, 
   coords.minBy(_.y).y - 1, coords.maxBy(_.y).y + 1)
}


object Grid {
  val (minX, maxX, minY, maxY) = findAxesExtrema(inputCoords)
  val xRange: List[Int] = List.range(minX, maxX + 1) 
  val yRange: List[Int] = List.range(minY, maxY + 1) 
  val coords = inputCoords
  val rawPoints: List[(Int, Int)] = { for (x <- xRange; y <- yRange) yield (x, y) }
  val gridSize = xRange.length * yRange.length

  def manhattanDistance(p1: (Int, Int), p2: (Int, Int)) = {
    abs(p1._1 - p2._1) + abs(p1._2 - p2._2)
  }
  
  def distances(x: Int, y: Int): List[(Coord, Int)] = coords.map { c => 
    (c, manhattanDistance((c.x, c.y), (x, y)))
  }

  lazy val coordsClosestToPoints: List[ClosestCoords] = {
    def closestCoords(x: Int, y: Int): (Int, List[Coord]) = {
      val distancesToPoint = distances(x, y)
      val minDistance: Int = distancesToPoint.minBy(_._2)._2
      val closestCoords: List[Coord] = distancesToPoint
        .filter { d => d._2 == minDistance }
        .map { d => d._1 }

      (minDistance, closestCoords)
    }

    rawPoints.map { p => 
      val (distance, coords) = closestCoords(p._1, p._2)
      ClosestCoords(p._1, p._2, distance, coords)
    }
  }

  def getClosestCoords(x: Int, y: Int) = { coordsClosestToPoints.filter { p => p.x == x && p.y == y }(0) }
  def getCoord(x: Int, y: Int) = { coords.filter { p => p.x == x && p.y == y }(0) }

  lazy val edgeClosestCoords: List[ClosestCoords] = {
    val xEdges = xRange.zip(List.fill(xRange.length) { minY }) ::: xRange.zip(List.fill(xRange.length) { maxY })
    val yEdges = List.fill(yRange.length) { minX }.zip(yRange) ::: List.fill(yRange.length) { maxX }.zip(yRange)
    val edges = xEdges ::: yEdges
    edges.map { p => getClosestCoords(p._1, p._2) }
  }

  lazy val unboundedCoords: List[Coord] = edgeClosestCoords
    .filter { p => p.coords.length == 1 }
    .map { p => p.coords(0) }
    .foldLeft(List[Coord]()) { (acc, p) =>
      if (! acc.contains(p)) p :: acc else acc
    }
  
  lazy val totalDistancesToAllCoords: List[TotalDistance] = {
    rawPoints.map { p =>
      TotalDistance(
        p._1, p._2, 
        distances(p._1, p._2).map { d => d._2 }.sum
      )
    }
  }
}

// part 1
def findBoundedAreaSizes() = {
  Grid.coordsClosestToPoints.filter { p => 
    ! (p.coords.length > 1 || Grid.unboundedCoords.contains(p.coords(0)))
  } 
    .groupBy(_.coords(0).id)
    .mapValues { l => l.length }
}

def findLargestBoundedArea() = {
  findBoundedAreaSizes().maxBy(_._2)
}

// part 2
def findSizeOfAreaWithLocalCoords() = {
  Grid.totalDistancesToAllCoords.filter { d => d.distance < LOCALITY }.length
}


val CALC_PART_1 = true
val CALC_PART_2 = true

if (CALC_PART_1) {
  println(s"Largest bounded area is (id, count): ${findLargestBoundedArea}")
}

if (CALC_PART_2) {
  println(s"Size of region with all local coords: ${findSizeOfAreaWithLocalCoords}")
}