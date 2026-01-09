object BeamSplitter extends App {
  val grid = io.Source
    .fromFile("7_beam_splitter/input.txt")
    .getLines()
    .toArray

  val (h, w) = (grid.length, grid.head.length)
  val sourceCol = grid.head.indexOf('S')
  val initialBeam = Set((0, sourceCol))

  val (part1, visited) = forward(initialBeam, 0, Set.empty)
  val part2 = backward(visited)

  println(s"Part 2: ${part1}")
  println(s"Part 1: ${part2}")

  def forward(
      beams: Set[(Int, Int)],
      count: Long,
      visited: Set[(Int, Int)]
  ): (Long, Set[(Int, Int)]) =
    if beams.isEmpty then (count, visited)
    else {
      val next = for (r, c) <- beams if r < h && c >= 0 && c < w
      yield grid(r)(c) match
        case '^' => ((r + 1, c - 1), (r + 1, c + 1), true)
        case _   => ((r + 1, c), (r + 1, c), false)
      val newBeams = next.flatMap { case (a, b, _) => List(a, b) }
      val newCount = count + next.count(_._3)
      forward(newBeams, newCount, visited ++ beams)
    }

  def backward(visited: Set[(Int, Int)]): BigInt =
    val finalState = Array.fill(w)(BigInt(1))
    (h - 2 to 0 by -1).foldLeft(finalState) { (below, r) =>
      (0 until w).map { c =>
        if !visited.contains((r, c)) then BigInt(0)
        else
          grid(r)(c) match
            case '^' =>
              val L = if c > 0 then below(c - 1) else BigInt(0)
              val R = if c < w - 1 then below(c + 1) else BigInt(0)
              L + R
            case _ => below(c)
      }.toArray
    }(sourceCol)
}