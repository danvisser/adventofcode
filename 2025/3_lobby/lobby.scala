object Lobby extends App {
  main()

  case class Bank(batteryRatings: List[Int])

  def main(): Unit = {
    val banks = io.Source
      .fromFile("3_lobby/input.txt")
      .getLines()
      .map(line => Bank(line.map(_.asDigit).toList))
      .toList

    val twoBatteryJoltage = banks.map(findMaximumJoltage(_, 2)).sum
    val twelveBatteryJoltage = banks.map(findMaximumJoltage(_, 12)).sum

    println(s"the sum of the largest two-digit numbers is $twoBatteryJoltage")
    println(
      s"the sum of the largest twelve-digit numbers is $twelveBatteryJoltage"
    )
  }

  def findMaximumJoltage(bank: Bank, batteriesToTurnOn: Int): Long = {
    val batteriesToRemove = bank.batteryRatings.length - batteriesToTurnOn

    if (bank.batteryRatings.length - batteriesToTurnOn <= 0) {
      bank.batteryRatings.take(batteriesToTurnOn).mkString.toLong
    } else {
      selectBatteriesGreedily(
        bank.batteryRatings,
        batteriesToTurnOn,
        batteriesToRemove
      )
    }
  }

  def selectBatteriesGreedily(
      batteryRatings: List[Int],
      batteriesToTurnOn: Int,
      batteriesToRemove: Int
  ): Long = {
    var selectedBatteries = List[Int]()
    var remainingBatteries = batteryRatings
    var remainingToRemove = batteriesToRemove

    for (position <- 0 until batteriesToTurnOn) {
      val batteriesNeeded = batteriesToTurnOn - position
      val canRemove = remainingBatteries.length - batteriesNeeded
      val removeNow = math.min(remainingToRemove, canRemove)

      val candidateWindow = remainingBatteries.take(removeNow + 1)
      val maxRating = candidateWindow.max
      val maxIndex = candidateWindow.indexOf(maxRating)

      selectedBatteries = selectedBatteries :+ maxRating
      remainingBatteries = remainingBatteries.drop(maxIndex + 1)
      remainingToRemove -= maxIndex
    }

    selectedBatteries.mkString.toLong
  }
}
