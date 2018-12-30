import scala.io.Source


val CALC_PART_1 = false 
val CALC_PART_2 = true 


val initialPolymer: List[Char] = Source.fromFile("./input")
  .mkString
  .replace("\n", "")
  .toList

def foldPolymer(polymer: List[Char]): List[Char] = {
  def adjacentsReact(a: Char, b: Char): Boolean = {
    if (a.toLower == b.toLower && a != b) true else false
  }

  def singlePass(unprocessedPart: List[Char], undestroyedPart: List[Char]): List[Char] = {
    unprocessedPart match {
      case Nil => 
        undestroyedPart.reverse
      case a :: b :: tail  => 
        val (nextUnprocessedPart, nextUndestroyedPart) = if (adjacentsReact(a, b)) {
          (tail, undestroyedPart)
        } else {
          // we prepend then reverse once at the end
          (b :: tail, a :: undestroyedPart)
        }
        singlePass(nextUnprocessedPart, nextUndestroyedPart)
      case head :: tail => 
        (head :: undestroyedPart).reverse
    }
  }
  
  val nextPolymer = singlePass(polymer, List())
  if (polymer.length == nextPolymer.length) nextPolymer else foldPolymer(nextPolymer)
}

// part 2
def findShortestPolymerWithUnitRemoved(polymer: List[Char]) = {
  foldPolymerForEachRemovedUnit(polymer).minBy(_._2)
}

def foldPolymerForEachRemovedUnit(polymer: List[Char]) = {
  // returns a list of tuples, where the first el of the tuple is the char that was removed, 
  // and the second element is the length of the polymer after it's been folded
  ('a' to 'z').map { c => (c, removeUnitAndFold(polymer, c).length) }
}

def removeUnitAndFold(polymer: List[Char], lowercaseUnit: Char) = {
  foldPolymer(removeUnit(polymer, lowercaseUnit))
}

def removeUnit(polymer: List[Char], lowercaseUnit: Char): List[Char] = {
  polymer.filter(c => c != lowercaseUnit && c != lowercaseUnit.toUpper)
}

if (CALC_PART_1) {
  val foldedPolymer = foldPolymer(initialPolymer)
  println(s"Parsed polymer is: $foldedPolymer")
  println(s"Its length is: ${foldedPolymer.length}")
}

if (CALC_PART_2) {
  val shortestPolymerWithUnitRemoved = findShortestPolymerWithUnitRemoved(initialPolymer)
  println(
    List(s"Removing '${shortestPolymerWithUnitRemoved._1}'' from the polymer gave the",
         s"shortest folded polymer, with length ${shortestPolymerWithUnitRemoved._2}."
    ).mkString(" ")
  )
}