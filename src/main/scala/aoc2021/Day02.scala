package aoc2021

object Day02 extends App {

  def solution1(instructions: List[String]): Int = {
    val (distance, depth) = instructions.foldLeft((0,0)) { case ((horizontal, depth), instruction) =>
      instruction match {
        case s"forward $num" => horizontal + num.toInt -> depth
        case s"up $num" => horizontal -> (depth - num.toInt)
        case s"down $num" => horizontal -> (depth + num.toInt)
      }
    }
    distance * depth
  }

  def solution2(instructions: List[String]): Int = {
    val (distance, depth, _) = instructions.foldLeft((0,0,0)) { case ((horizontal, depth, aim), instruction) =>
      instruction match {
        case s"forward $num" => (horizontal + num.toInt, depth + (aim * num.toInt), aim)
        case s"up $num" => (horizontal, depth, aim - num.toInt)
        case s"down $num" => (horizontal, depth, aim + num.toInt)
      }
    }
    distance * depth
  }

  val instructions = io.Source.fromResource("2021/day02.txt").getLines.toList

  println(solution1(instructions))
  println(solution2(instructions))
}
