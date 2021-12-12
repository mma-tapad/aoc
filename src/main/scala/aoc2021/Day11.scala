package aoc2021

import aoc2021.Utils.Point

object Day11 extends App {
  def incrementEnergy(curr: Point, grid: Array[Array[Int]])(counter: Int = 0, flashed: Set[Point] = Set.empty): (Int, Set[Point]) = {
    grid(curr.y)(curr.x) match {
      case 9 =>
        grid(curr.y)(curr.x) = 0
        val neighbors = curr.findValidNeighbors(grid.head.length, grid.length)() ++
          curr.findDiagonals(grid.head.length, grid.length)
        neighbors.foldLeft((counter+1, flashed + curr)) { case ((counterAcc, flashedAcc), neighbor) =>
          incrementEnergy(neighbor, grid)(counterAcc, flashedAcc)
        }
      case num =>
        if(flashed.contains(curr)) {
          counter -> flashed
        } else {
          grid(curr.y)(curr.x) = num + 1
          counter -> flashed
        }
    }
  }


  def solution1(lines: Array[String]): Int = {
    val grid: Array[Array[Int]] = lines.map(_.toCharArray.map(_.asDigit))

    (0 until 100).map { _ =>
      grid.indices.foldLeft(0, Set.empty[Point]) { case ((yCounterAcc, yFlashedAcc), y) =>
        grid.head.indices.foldLeft((yCounterAcc, yFlashedAcc)) { case ((xCounterAcc, xFlashedAcc), x) =>
          incrementEnergy(Point(x, y), grid)(xCounterAcc, xFlashedAcc)
        }
      }._1
    }.sum
  }

    def solution2(lines: Array[String]): Option[Long] = {
      val grid: Array[Array[Int]] = lines.map(_.toCharArray.map(_.asDigit))

      (0 until Long.MaxValue).find { _ =>
        grid.indices.foldLeft(0, Set.empty[Point]) { case ((ycounterAcc, yflashedAcc), y) =>
          grid.head.indices.foldLeft((ycounterAcc, yflashedAcc)) { case ((xcounterAcc, xflashedAcc), x) =>
            incrementEnergy(Point(x, y), grid)(xcounterAcc, xflashedAcc)
          }
        }

        grid.forall(_.forall(_ == 0))
      }
    }

  val lines = io.Source.fromResource("2021/day11.txt").getLines.toArray

  println(solution1(lines))
  println(solution2(lines))
}
