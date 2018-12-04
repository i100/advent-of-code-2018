import scala.io.Source
import scala.util.{ Try, Success, Failure } 


case class Claim(
  number: Int, 
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
    val pattern(number, leftOffset, topOffset, width, height) = s
    Claim(number.toInt, leftOffset.toInt, topOffset.toInt, width.toInt, 
          height.toInt) 
  }
}


// this is a bit crap -- mutate eeeet!!
// represents a square inch on the grid
class Point(x: Int, y: Int) {
  var touches: Int = 0
  def touch() = { touches = touches + 1}
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

  def countTouched(minTouches: Int = 2): Int = {
    val touchesPoints: List[Int] = getAllPoints.map { p =>
      if (p.touches >= minTouches) 1 else 0
    }

    touchesPoints.sum
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
    // println(s"Updating grid with claim# ${claim.number}")    // println(s"Updating grid with claim# ${claim.number}")    // println(s"Updating grid with claim# ${claim.number}")    (claim.leftOffset to (claim.leftOffset + claim.width)).toList.map { x =>
    width.map { x =>  
      height.map { y =>
        Grid.getPoint(x, y).touch 
      }
    }
  } 

  claims.foreach(updateClaimedPoints(_))
}

updateGrid(getClaims())
println(s"Touched points: ${Grid.countTouched(2)}")