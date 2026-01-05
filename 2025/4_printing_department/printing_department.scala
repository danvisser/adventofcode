object PrintingDepartment extends App {
  private val Roll = '@'
  private val Empty = '.'

  val initialGrid = io.Source
    .fromFile("4_printing_department/input.txt")
    .getLines()
    .map(_.toArray)
    .toArray

  val part1 = removeRolls(minNeighbors = 4, initialGrid)._2
  val part2 = keepRemovingRolls(minNeighbors = 4, initialGrid)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")

  private def countRollNeighbors(
      grid: Array[Array[Char]],
      row: Int,
      col: Int
  ): Int = {
    val neighbors = for {
      dr <- -1 to 1
      dc <- -1 to 1
      if dr != 0 || dc != 0
      r = row + dr
      c = col + dc
      if r >= 0 && r < grid.length
    } yield grid(r)(c)
    neighbors.count(_ == Roll)
  }

  private def removeRolls(
      minNeighbors: Int,
      grid: Array[Array[Char]]
  ): (Array[Array[Char]], Int) = {
    var count = 0
    val newGrid = grid.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (cell, c) =>
        if (cell == Roll && countRollNeighbors(grid, r, c) < minNeighbors) {
          count += 1
          Empty
        } else cell
      }.toArray
    }.toArray
    (newGrid, count)
  }

  private def keepRemovingRolls(
      minNeighbors: Int,
      grid: Array[Array[Char]],
      removed: Int = 0
  ): Int = {
    val (newGrid, count) = removeRolls(minNeighbors, grid)
    if (count == 0) removed
    else keepRemovingRolls(minNeighbors, newGrid, removed + count)
  }
}
