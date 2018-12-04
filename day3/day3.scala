import scala.io.Source
import scala.util.{ Try, Success, Failure } 
import scala.collection.mutable.ListBuffer

case class Claim(
  id: Int, 
  leftOffset: Int, 
  topOffset: Int, 
  width: Int, 
  height: Int
)

def getClaims(): Seq[Claim] = {
  val raw: List[String] = Source.fromFile("./input")
    .getLines
    .toList

  // extract the stuff with regex
  val pattern = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
  
  raw.map { s =>
    val pattern(id, leftOffset, topOffset, width, height) = s
    Claim(id.toInt, leftOffset.toInt, topOffset.toInt, width.toInt, 
          height.toInt) 
  }
}


// this is a bit crap -- mutate eeeet!!
// represents a square inch on the grid
class Point(x: Int, y: Int) {
  var touchedBy: ListBuffer[Int] = ListBuffer()
  def touch(by: Int) = { touchedBy += by }
}

object Grid {
  // I'm not sure how big the grid is but, looking at the data, 1024 x 1024 seems
  // like it should cover it... we really should let the grid expand as we see 
  // new data but this is easier!
  val width = 1024
  val height = 1024
  val widthRange = (0 to width)
  val heightRange = (0 to height)

  val rawPoints: Array[Array[Point]] = {
    widthRange.toList.map { x =>
      heightRange.toList.map { y => 
        new Point(x, y)
      }.toArray
    }.toArray
  }

  def getPoint(x: Int, y: Int): Point = { rawPoints(x)(y) }

  def getAllPoints(): List[Point] = {
    widthRange.toList.map { x =>
      heightRange.toList.map { y =>
        Grid.getPoint(x, y) 
      }
    }.flatten
  }     

  def getOnceTouchedPoints(): List[Point] = {
    getAllPoints.filter(_.touchedBy.length == 1)
  }

  def getDoubleTouchedPoints(): List[Point] = {
    getAllPoints.filter(_.touchedBy.length >= 2)
  }
  
  def showTouches() = {
    for (p <- getAllPoints) { println(p.touchedBy) }
  }
}

def updateGrid(claims: Seq[Claim]) = {
  def updateClaimedPoints(claim: Claim) = {
    // println(s"Updating grid with claim# ${claim.number}")
    val width = (claim.leftOffset to claim.leftOffset + claim.width - 1).toList
    val height = (
      Grid.height - claim.topOffset - claim.height + 1 
      to Grid.height - claim.topOffset
    ).toList

    width.map { x =>  
      height.map { y =>
        Grid.getPoint(x, y).touch(claim.id)
      }
    }
  } 

  claims.foreach(updateClaimedPoints(_))
}

def findSantasGoodJersey(): Set[Int] = {
  val onceTouchedClaims: Set[Int] = Grid.getOnceTouchedPoints
    .map(_.touchedBy)
    .flatten
    .toSet

  val doubleTouchedClaims: Set[Int] = Grid.getDoubleTouchedPoints
    .map(_.touchedBy)
    .flatten
    .toSet
  
  onceTouchedClaims &~ doubleTouchedClaims
}

updateGrid(getClaims())

println(s"Double touched points: ${Grid.getDoubleTouchedPoints.length}")
println(s"Santa's good jersey is #: ${findSantasGoodJersey}")