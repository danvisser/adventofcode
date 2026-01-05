object printingDepartment extends App {
  main()

  case class State(code: Int, passesThroughZero: Int)

  def main(): Unit = {

    val states = io.Source
      .fromFile("4_printing_department/input.txt")
      .getLines()
      .flatMap(parseRills)
      .foreach(println)
  }

  def parseRills(line: String): List[String] = {
    line.split("\\s+").toList
  }
}
