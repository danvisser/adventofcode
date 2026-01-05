import scala.util.chaining.*

object PrintingDepartment extends App {
  val Roll = '@'
  val InputPath = "4_printing_department/input.txt"

  io.Source.fromFile(InputPath)
  .getLines()
  .map(_.toArray)
  .toArray
  .pipe { grid =>
    grid.indices.flatMap(i =>
      grid(i).indices.collect {
        case j if grid(i)(j) == Roll =>
          val adj = neighbors(i, j, grid).count(_ == Roll)
          if (adj < 4) 1 else 0
      }
    ).sum
  }
  .pipe(println)

  def neighbors(row: Int, col: Int, grid: Array[Array[Char]]): Seq[Char] =
    for {
      dr <- -1 to 1
      dc <- -1 to 1
      if dr != 0 || dc != 0
      r = row + dr
      c = col + dc
      if r >= 0 && r < grid.length
      if c >= 0 && c < grid(r).length
    } yield grid(r)(c)

}