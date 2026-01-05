object GiftShop extends App {
  main()

  case class IdRange(firstID: Long, lastID: Long)
  case class RepeatingId(ID: Long, count: Int)

  def main(): Unit = {
    val repeatingIds = io.Source
      .fromFile("2_gift_shop/input.txt")
      .getLines()
      .map(parseIdRanges)
      .flatMap(findRepeatingIds)
      .toList

    val twiceRepeats = repeatingIds.filter(_.count == 2).map(_.ID).sum
    val allRepeats = repeatingIds.map(_.ID).sum

    println("the sum of twice-repeating numbers is " + twiceRepeats)
    println("the sum of the repeats is " + allRepeats)
  }

  def parseIdRanges(line: String): List[IdRange] = {
    line
      .split(",")
      .map(range =>
        IdRange(
          range.split("-").map(_.toLong).head,
          range.split("-").map(_.toLong).last
        )
      )
      .toList
  }

  def findRepeatingIds(ranges: List[IdRange]): List[RepeatingId] = {
    ranges.flatMap { range =>
      (range.firstID to range.lastID).flatMap { id =>
        getMinRepeatingPatternCount(id.toString).map(repeatCount =>
          RepeatingId(id, repeatCount)
        )
      }
    }
  }

  def getMinRepeatingPatternCount(s: String): Option[Int] = {
    val len = s.length
    (len / 2 to 1 by -1)
      .find { patternLength =>
        len % patternLength == 0
        && (len / patternLength) >= 2
        && hasRepeatingPattern(patternLength, s)
      }
      .map(len / _)
  }

  def hasRepeatingPattern(patternLength: Int, s: String): Boolean = {
    val pattern = s.substring(0, patternLength)
    (1 until s.length / patternLength).forall { i =>
      val start = i * patternLength
      s.substring(start, start + patternLength) == pattern
    }
  }
}
