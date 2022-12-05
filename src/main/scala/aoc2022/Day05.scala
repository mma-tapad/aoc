package aoc2022

import scala.annotation.tailrec

object Day05 extends App {

  /**
   * Given a row of boxes, group by 4 chars to separate each box into its own group (ie "[x] "). Requires trimming.
   * Each group represents one box that exists in a stack, starting from stack #1.
   * If the group does not match the pattern, that stack doesn't have a box in that row.
   */
  @tailrec
  def parseBoxes(boxes: List[String])
                (resMap: Map[String, List[Char]] = Map.empty.withDefaultValue(Nil)): Map[String, List[Char]] = {
    boxes match {
      case Nil             => resMap
      case headBox :: tail =>
        val updatedMap = headBox
          .grouped(4)
          .map(_.trim)
          .foldLeft(1, resMap) {
            case ((keyCount, rMap), s"[$c]") =>
              keyCount+1 -> rMap.updated(keyCount.toString, c.head :: rMap(keyCount.toString))
            case ((keyCount, rMap), _) =>
              keyCount+1 -> rMap
          }
          ._2

        parseBoxes(tail)(updatedMap)
    }
  }

  /**
   * Given boxes: we call parseBoxes above to give us our starting state.
   * Given instructions:
   *   if we cannot batch grab boxes, pick up the amount directed, and reverse it to simulate picking up one at a time.
   *   If we can batch grab boxes, no need to reverse. Just take and prepend to destination stack.
   * Finally, read the top box of every stack.
   */
  def simulateMovesAndReadTop(boxes: List[String], instructions: List[String], canBatch: Boolean): String = {
    val stacks = instructions
      .foldLeft(parseBoxes(boxes)()) { case (resStack, s"move $num from $src to $dest") =>
        val toMove = {
          val m = resStack(src).take(num.toInt)
          if(canBatch) m
          else m.reverse
        }
        resStack
          .updated(src, resStack(src).drop(toMove.length))
          .updated(dest, toMove ::: resStack(dest))
      }

    (1 to stacks.size).foldLeft("") { case (acc, key) =>
      acc + stacks(key.toString).head
    }
  }

  def solution1(boxes: List[String], instructions: List[String]): String = {
    simulateMovesAndReadTop(boxes, instructions, canBatch = false)
  }
  def solution2(boxes: List[String], instructions: List[String]): String = {
    simulateMovesAndReadTop(boxes, instructions, canBatch = true)
  }

  val text = io.Source.fromResource("2022/day05.txt").getLines.toList
  val boxes = text.takeWhile(!_.startsWith(" 1")).reverse
  val instructions = text.filter(_.startsWith("move"))

  println(solution1(boxes, instructions))
  println(solution2(boxes, instructions))
}
