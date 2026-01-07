object TrashCompactor extends App {
  private val Empty = ' '
  private val Marker = "-"

  val file = io.Source
    .fromFile("./6_trash_compactor/input.txt")
    .mkString
    .split("\n")
    .toList

  val part1Input = file.map(_.toString.split("\\s+").toList)
  val part2input = markEmptyCols(file).map(_.trim.split("\\s+").toList)

  val part1 = part1Input.init
    .transpose
    .map(_.map(BigInt(_)))
    .lazyZip(part1Input.last)
    .map((ns, o) => ns.reduceLeft(if (o == "+") _ + _ else _ * _))
    .sum

  val part2 = part2input.init
    .map(_.map(_.toList))
    .transpose
    .map(g => g.head.indices.map(i => g.map(_(i))).toList)
    .map(_.map(c => BigInt(c.mkString.replace(Marker, ""))))
    .lazyZip(part2input.last)
    .map((ns, o) => ns.reduceRight(if (o == "+") _ + _ else _ * _))
    .sum

  println(part1)
  println(part2)

  def markEmptyCols(f: List[String]) = {
    val mask = f.transpose.map(_.exists(_ != Empty))
    f.init.map(
      _.zip(mask)
        .map(x =>
          x match {
            case (Empty, true) => Marker
            case (c, _)        => c
          }
        )
        .mkString
    ) :+ f.last
  }
}