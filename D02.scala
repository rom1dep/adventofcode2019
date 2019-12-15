package aoc19

import scala.annotation.tailrec

object D02 extends App with
  val input                = scala.io.Source.fromFile("./c2").getLines.toList.head
  val data                 = input.split(',').map(_.toInt)
  val initProgram          = data.updated(1, 12).updated(2, 2).toList
  type Program = List[Int]

  @tailrec
  def compute(cursor: Int = 0, state: Program = initProgram): Program =
    val op = state(cursor)
    op match
      case 1|2 =>
        val reg1 = state(cursor + 1)
        val reg2 = state(cursor + 2)
        val dest = state(cursor + 3)
        compute(cursor + 4, if op == 1
        then state.updated(dest, state(reg1) + state(reg2))
        else state.updated(dest, state(reg1) * state(reg2)))
      case 99 => state

  //Solution 1
  println(compute()(0))

  //Solution 2 "what pair of inputs produces the output 19690720"
  val nouns = (0 to 99)
  val verbs = (0 to 99)
  val res =
    for n <- nouns.view
        v <- verbs.view
        newProgram = data.updated(1, n).updated(2, v).toList
        output = compute(cursor = 0, newProgram)(0)
    yield (output, 100 * n + v)
  println(res.dropWhile(_._1 != 19690720).head._2)
