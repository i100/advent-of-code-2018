import scala.io.Source


val SUM_ALL_FREQUENCIES = false 
val TEST_FOR_REPETITION = true

def sumFrequencies(freqs: List[Int]): Int = {
  sumFrequencies(0, freqs)
}

def sumFrequencies(currentFreq: Int, freqs: List[Int]): Int = {
  freqs match {
    case Nil => currentFreq
    case nextFreq :: tail => 
      sumFrequencies(currentFreq + nextFreq, tail)
  } 
}

def findFirstRepeatedFrequency(freqs: List[Int]): Int = {
  findFirstRepeatedFrequency(0, freqs, List(0))
}

def findFirstRepeatedFrequency(currentFreq: Int, 
    freqsRemaining: List[Int], freqsSeen: List[Int]): Int = {

  freqsRemaining match {
    case Nil =>
      // get the list again and keep iterating
      findFirstRepeatedFrequency(currentFreq, 
        frequencyList, freqsSeen)
    case nextFreq :: tail =>
      val updatedFreq = currentFreq + nextFreq

      // ? do it with a hash
      if (freqsSeen.contains(updatedFreq)) {
        updatedFreq
      } else {
        findFirstRepeatedFrequency(updatedFreq, tail, 
          updatedFreq :: freqsSeen)
      }
  }
}

val frequencyList: List[Int] = Source.fromFile("./input")
  .getLines
  .toList
  .map { _.toInt }

if (SUM_ALL_FREQUENCIES) { 
  println(s"New frequency: ${sumFrequencies(frequencyList)}")
}

if (TEST_FOR_REPETITION) {
  val freq = findFirstRepeatedFrequency(frequencyList)
  println(s"First repeated frequency: ${freq}")
}