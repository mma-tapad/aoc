package aoc2021

import aoc2021.Utils.Point

object Day09 extends App {
  def findBasin(bottom: Point, visitedPoints: Set[Point])
               (grid: Seq[Seq[Int]]): Set[Point] = {
    val neighbors = bottom.findValidNeighbors(grid.head.size, grid.size) { neighbor =>
      val neighborValue = grid(neighbor.y)(neighbor.x)
      !visitedPoints.contains(neighbor) && neighborValue < 9 && neighborValue > grid(bottom.y)(bottom.x)
    }
    neighbors match {
        case Nil => visitedPoints + bottom
        case list => list.foldLeft(visitedPoints) { case (visited, p) =>
          visited ++ findBasin(p, visited + bottom)(grid)
        }
      }
  }

  def solution1(grid: Seq[Seq[Int]]): Int = {
    grid.indices.flatMap { y =>
      grid.head.indices.flatMap { x =>
        val pointValue = grid(y)(x)

        Point(x, y).findValidNeighbors(grid.head.size, grid.size) { neighbor =>
          grid(neighbor.y)(neighbor.x) <= pointValue
        } match {
          case Nil => Some(pointValue + 1)
          case _   => None
        }
      }
    }.sum
  }

  def solution2(grid: Seq[Seq[Int]]): Int = {
    grid.indices.flatMap { y =>
      grid.head.indices.flatMap { x =>
        val point = Point(x,y)

        point.findValidNeighbors(grid.head.size, grid.size) { neighbor =>
          grid(neighbor.y)(neighbor.x) > grid(y)(x)
        } match {
          case Nil => None
          case _   => Some(findBasin(point, Set.empty[Point])(grid).size)
        }
      }
    }
    .sorted
    .takeRight(3)
    .product
  }

  val grid = io.Source.fromResource("2021/day09.txt").getLines.toSeq.map(_.toSeq.map(_.asDigit))

  println(solution1(grid))
  println(solution2(grid))
}
