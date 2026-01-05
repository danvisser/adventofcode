object SecretEntrance extends App {
  main()

  case class State(code: Int, passesThroughZero: Int)

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction

  case class Rotation(direction: Direction, amount: Int)

  def main(): Unit = {
    val initialState = State(code = 50, passesThroughZero = 0)

    val states = io.Source
      .fromFile("1_secret_entrance/input.txt")
      .getLines()
      .map(parseRotation)
      .scanLeft(initialState)(applyRotation)
      .toList

    val zeroStates = states.count(_.code == 0)
    val passesThroughZero = states.map(_.passesThroughZero).sum

    println(s"Number of zeros: $zeroStates")
    println(s"Number of passes through zero: $passesThroughZero")
  }

  def parseRotation(line: String): Rotation = {
    val direction = if (line.head == 'L') Left else Right
    val amount = line.tail.toInt
    Rotation(direction, amount)
  }

  def applyRotation(state: State, rotation: Rotation): State =
    rotation.direction match {
      case Left  => rotateLeft(state, rotation.amount)
      case Right => rotateRight(state, rotation.amount)
    }

  def rotateLeft(state: State, amount: Int): State = {
    val rawValue = state.code - amount
    val newValue = (rawValue % 100 + 100) % 100
    val basePasses = math.abs(rawValue) / 100
    val crossesZero = if (newValue > 0 && rawValue < 0) 1 else 0
    val landsOnZero = if (rawValue == 0) 1 else 0
    State(newValue, basePasses + crossesZero + landsOnZero)
  }

  def rotateRight(state: State, amount: Int): State = {
    val rawValue = state.code + amount
    val newValue = rawValue % 100
    val passes = rawValue / 100
    State(newValue, passes)
  }
}
