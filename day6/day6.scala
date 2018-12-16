import scala.io.Source
import scala.math.abs 


abstract class GridPoint() { 
  val x: Int 
  val y: Int
}

case class Coord(x: Int, y: Int, id: Int) extends GridPoint { }
case class Location(
  x: Int, 
  y: Int, 
  manHattanDistance: Int,
  closestCoords: List[Coord]
) extends GridPoint { } 

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
  val gridSize = xRange.length * yRange.length

  lazy val locations: List[Location] = {
    // the names are very confusing :(
    def xyIsCoord(x:Int, y: Int): Boolean = {
      (coords.filter { c => c.x == x && c.y == y }.length >= 1)
    }

    val rawPoints: List[(Int, Int)] = {
      for (x <- xRange; y <- yRange) yield (x, y) // if ! xyIsCoord(x, y)) yield (x, y)
    }

    def closestCoords(x: Int, y: Int): (Int, List[Coord]) = {
      def manhattanDistance(p1: (Int, Int), p2: (Int, Int)) = {
        abs(p1._1 - p2._1) + abs(p1._2 - p2._2)
      }
      // ouch
      val distances: List[(Coord, Int)] = coords.map { c => 
        (c, manhattanDistance((c.x, c.y), (x, y)))
      }
      val minDistance: Int = distances.minBy(_._2)._2
      val closestCoords: List[Coord] = distances
        .filter { d => d._2 == minDistance }
        .map { d => d._1 }

      (minDistance, closestCoords)
    }

    rawPoints.map { p => 
      val (distance, coords) = closestCoords(p._1, p._2)
      Location(p._1, p._2, distance, coords)
    }
  }

  def getLocation(x: Int, y: Int) = { locations.filter { p => p.x == x && p.y == y }(0) }
  def getCoord(x: Int, y: Int) = { coords.filter { p => p.x == x && p.y == y }(0) }

  lazy val edgeLocations: List[Location] = {
    val xEdges = xRange.zip(List.fill(xRange.length) { minY }) ::: xRange.zip(List.fill(xRange.length) { maxY })
    val yEdges = List.fill(yRange.length) { minX }.zip(yRange) ::: List.fill(yRange.length) { maxX }.zip(yRange)
    val edges = xEdges ::: yEdges
    edges.map { p => getLocation(p._1, p._2) }
  }

  lazy val unboundedCoords: List[Coord] = edgeLocations
    .filter { p => p.closestCoords.length == 1 }
    .map { p => p.closestCoords(0) }
    .foldLeft(List[Coord]()) { (acc, p) =>
      if (! acc.contains(p)) p :: acc else acc
    }
}

def findBoundedAreaSizes() = {
  Grid.locations.filter { p => 
    ! (p.closestCoords.length > 1 || Grid.unboundedCoords.contains(p.closestCoords(0)))
  } 
    .groupBy(_.closestCoords(0).id)
    .mapValues { l => l.length }
}

def findLargestBoundedArea() = {
  findBoundedAreaSizes().maxBy(_._2)
}

val CALC_PART_1 = true
val CALC_PART_2 = true

if (CALC_PART_1) {
  println(s"Largest bounded area is (id,count): ${findLargestBoundedArea}")
}

if (CALC_PART_2) {

}