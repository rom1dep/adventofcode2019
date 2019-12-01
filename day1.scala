val input                = scala.io.Source.fromFile("./c1")
val data                 = input.getLines().toList.map(_.toInt)
def fuel(mass: Int): Int = ((mass / 3).floor - 2).max(0).toInt

// Part 1
val answer1 = data.map(fuel).sum
println(f"answer part 1: $answer1")

// Part 2
val answer2 = data.map { m =>
  lazy val fuelMass: LazyList[Int] = m #:: fuelMass.map(fuel)
  fuelMass.takeWhile(_ != 0).sum - m
}.sum
println(f"answer part 2: $answer2")
