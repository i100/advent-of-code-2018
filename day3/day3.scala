import scala.io.Source
import scala.util.{ Try, Success, Failure } 

val claims: List[String] = Source.fromFile("./input")
  .getLines
  .toList