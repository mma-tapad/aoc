package aoc

import scala.annotation.tailrec
import scala.math.BigDecimal.double2bigDecimal

object Day24 extends App {
  val regex = """(e|se|ne|w|sw|nw)""".r
  val coordMap: Map[String, (Double, Double)] = Map(
    "e"  -> (1D -> 0D),
    "se" -> (0.5 -> -1D),
    "ne" -> (0.5 -> 1D),
    "w"  -> (-1D -> 0D),
    "sw" -> (-0.5 -> -1D),
    "nw" -> (-0.5 -> 1D)
  )

  def tileSetup(tileDirections: Seq[String]): Set[(Double, Double)] = {
    def directionToCoord(direction: String): (Double, Double) = {
      regex
        .findAllIn(direction)
        .map(coordMap)
        .foldLeft((0D, 0D)) { case ((accX, accY), (x, y)) =>
          (accX + x, accY + y)
        }
    }

    tileDirections
      .map(directionToCoord)
      .foldLeft(Set.empty[(Double, Double)]) { case (acc, tile) =>
        if(acc.contains(tile)) acc excl tile
        else acc incl tile
      }
  }

  def solution1(tileDirections: Seq[String]): Int = {
    tileSetup(tileDirections).size
  }

  def solution2(tileDirections: Seq[String]): Int = {
    def findNeighbors(coord: (BigDecimal, BigDecimal)): Seq[(BigDecimal, BigDecimal)] = {
      coordMap
        .values
        .toSeq
        .map { case (x, y) =>
          (coord._1 + x, coord._2 + y)
        }
    }

    @tailrec
    def play(blackTiles: Set[(BigDecimal, BigDecimal)], numIterations: Int): Set[(BigDecimal, BigDecimal)] = {
      numIterations match {
        case 0 => blackTiles
        case _ =>
          val newSetup = blackTiles
            .flatMap { tile =>
              val neighbors = findNeighbors(tile)
              val blackNeighbors = neighbors.count(blackTiles.contains)

              neighbors.filter { neighbor =>
                findNeighbors(neighbor).count(blackTiles.contains) == 2
              } ++ (
                if(blackNeighbors == 1 || blackNeighbors == 2) Seq(tile)
                else Seq.empty
              )
            }

          play(newSetup, numIterations-1)
      }
    }

    val blackTiles = tileSetup(tileDirections)
      .map { case (x, y) =>
        BigDecimal(x) -> BigDecimal(y)
      }

    play(blackTiles, 100).size

  }

  val tileDirections = io.Source.fromResource("day24.txt").getLines.toSeq

  println(solution1(tileDirections))
  println(solution2(tileDirections))
}
