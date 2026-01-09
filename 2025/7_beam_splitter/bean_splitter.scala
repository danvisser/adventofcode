import scala.io.Source

object BeamSplitter extends App {
  val grid = Source
    .fromFile("7_beam_splitter/input.txt")
    .getLines()
    .toArray

  val (h, w) = (grid.length, grid.head.length)
  val initialBeam = Set((1, grid.head.indexOf('S')))
  val part1 = forward(initialBeam, 0)

  println(s"Part 1: ${part1}")

  def forward(beams: Set[(Int, Int)], count: Long): Long =
    if beams.isEmpty then count
    else {
      val next =
        for (r, c) <- beams if r < h && c >= 0 && c < w      // filter beams outside grid
        yield grid(r)(c) match
          case '^' => ((r + 1, c - 1), (r + 1, c + 1), true) // split the beam, otherwise
          case _   => ((r + 1, c), (r + 1, c), false)        // move the beam forward
      val newBeams = next.flatMap { case (a, b, _) => List(a, b) }
      val newCount = count + next.count(_._3)
      forward(newBeams, newCount)
    }
}