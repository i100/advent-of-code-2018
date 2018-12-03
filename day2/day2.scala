import scala.io.Source
import scala.util.{ Try, Success, Failure } 


val CALC_CHECKSUM = false 
val TEST_ONE_CHAR_DIFF = true 

val boxIDs: List[String] = Source.fromFile("./input")
  .getLines
  .toList

def rudimentaryChecksum(strs: List[String]): Int = {  
  // a map of 2's and 3's to their counts, like: 
  // Map(2 -> 2, 3 -> 1)
  val counts: Map[Int, Int] = strs.flatMap {
    _.toList
    .groupBy(identity) // count each letter
    .map(_._2.length)
    .filter({ i => i == 2 || i == 3 })
    .toList
    .distinct
  } .groupBy(identity) // group by counts
    .map { case (k, v) => (k, v.length) }

  counts.getOrElse(2, 0)* counts.getOrElse(3, 0)
}

def stringsWithOneCharDifference(strs: List[String]): 
    (String, String) = {
  def countDiff(l1: List[Char], l2: List[Char], 
      diffCount: Int = 0): Int = {

    // println(s"Scanning $l1 vs $l2") 

    l1 match {
      case Nil => 
        println(s"Reached end with diffcount ${diffCount}")
        diffCount
      case head :: tail => 
        if (head == l2.head) {
          countDiff(tail, l2.tail, diffCount)
        } else {
          if (diffCount > 1) {
            diffCount
          } else {
            countDiff(tail, l2.tail, diffCount + 1)
          }
        }
    }
  }

  def searchForMatch(s: String, strs: List[String]): String = {
    val s2 = strs.head
    if (countDiff(s.toList, s2.toList) == 1) {
      s2
    } else {
      searchForMatch(s, strs.tail)
    }
  }

  Try(searchForMatch(strs.head, strs.tail)) match {
    case Success(s) => (strs.head, s.toString)
    case Failure(e) => 
      if (strs.tail.isEmpty) throw new Exception("No match found") 
      stringsWithOneCharDifference(strs.tail)
  }
}

println(s"Box IDs: $boxIDs")

if (CALC_CHECKSUM) {
  println(s"Checksum: ${rudimentaryChecksum(boxIDs)}")
}

if (TEST_ONE_CHAR_DIFF) {
  val res = stringsWithOneCharDifference(boxIDs)
  println(s"Strings with one char diff: ${res._1}, ${res._2}")
}