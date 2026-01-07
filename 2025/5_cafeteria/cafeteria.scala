object Cafeteria extends App {

  val input = io.Source
    .fromFile("5_cafeteria/input.txt")
    .mkString
    .split("\n\n")
    .toList

  val freshRanges = input(0)
    .split("\n")
    .map(_.split("-").map(BigInt(_)).toList)
    .toList
    .sortBy(_(0))
    .foldLeft(List[List[BigInt]]()) {
      case (Nil, curr)          => List(curr)
      case (last :: rest, curr) =>
        if (curr(0) <= last(1)) {
          List(last(0), last(1).max(curr(1))) :: rest
        } else {
          curr :: last :: rest
        }
    }

  val ingredients = input(1)
    .split("\n")
    .map(BigInt(_))
    .toList

  val part1 = ingredients
    .filter(x =>freshRanges.exists(r => x >= r(0) && x <= r(1)))
    .length

  val part2 = freshRanges
    .map(r => r(1) - r(0) + 1)
    .sum

  println(part1)
  println(part2)
}