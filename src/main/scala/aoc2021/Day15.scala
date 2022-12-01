package aoc2021

import aoc2021.Utils.Point
import scala.annotation.tailrec

object Day15 extends App {
  type Risk = Int

  @tailrec
  def exploreRisks(to: Point, from: Option[Point])
                  (maxX: Int, maxY: Int,
                   shortestDistances: Map[Point, Risk],
                   pathsVisited: Set[(Point, Point)],
                   pathsToVisit: List[(Point, Point)],
                   risksByPoint: Map[Point, Risk]
                  ): (Map[Point, Risk], Set[(Point, Point)], List[(Point, Point)]) = {

    val totalRisk = from.fold(0)(shortestDistances(_) + risksByPoint(to))
    val updatedPathsVisited = from.fold(pathsVisited)(p => pathsVisited.incl((p, to)))

    // if we've been here before for cheaper, move on.
    // if this is the cheapest we've made it here, update my shortest path,
    // but also update the shortest paths to our neighbors if we've been to them through us already
    val updatedShortestDistances = shortestDistances.get(to) match {
      case Some(dist) if totalRisk >= dist => shortestDistances
      case _                               =>
        to
          .findValidNeighbors(maxX, maxY)(neighbor => updatedPathsVisited.contains((to, neighbor)))
          .foldLeft(shortestDistances.updated(to, totalRisk)) { case (dists, visitedNeighbor) =>
            val possibleNeighborDistance = totalRisk + risksByPoint(visitedNeighbor)
            if(possibleNeighborDistance < dists(visitedNeighbor)) {
              dists.updated(visitedNeighbor, possibleNeighborDistance)
            } else {
              dists
            }
          }
    }

    // if we've run out of paths to consume, done
    // if we have neighbors, put them in the queue and explore one neighbor
    // if we have paths in the queue, explore them
    (to.findValidNeighbors(maxX, maxY)(neighbor => !pathsVisited.contains((to, neighbor))), pathsToVisit) match {
      case (Nil, Nil) => (updatedShortestDistances, updatedPathsVisited, pathsToVisit)
      case (headNeighbor :: tail, paths) =>
        val newPathsToVisit = tail.map(to -> _)
        exploreRisks(headNeighbor, Some(to))(maxX, maxY, updatedShortestDistances, updatedPathsVisited, paths ++ newPathsToVisit, risksByPoint)
      case (Nil, (from, to) :: tail) =>
        exploreRisks(to, Some(from))(maxX, maxY, updatedShortestDistances, updatedPathsVisited, tail, risksByPoint)
    }
  }

  def helper(grid: Array[Array[Risk]]): Risk = {
    val maxX = grid.head.length
    val maxY = grid.length
    val risksByPoint: Map[Point, Risk] = grid.indices.flatMap { y =>
      grid.head.indices.map { x =>
        Point(x,y) -> grid(y)(x)
      }
    }.toMap

    val (shortestDists, _, _) = exploreRisks(Point(0,0), None)(maxX, maxY, Map.empty, Set.empty, Nil, risksByPoint)
    shortestDists(Point(maxX-1, maxY-1))
  }

  def solution1(grid: Array[Array[Risk]]): Risk = {
    helper(grid)
  }

  def solution2(grid: Array[Array[Risk]]): Risk = {
    def expandGrid(grid: Array[Array[Risk]]): Array[Array[Risk]] = {
      val maxX = grid.head.length
      val maxY = grid.length
      (0 until maxY * 5).toArray.map { y =>
        (0 until maxX * 5).toArray.map { x =>
          val iteration = (x / maxX) + (y / maxY)
          val newValue = grid(y % maxY)(x % maxX) + iteration

          newValue % 10 match {
            // after rolling over from 10, it starts at 1, not 0.
            case rollover if rollover < newValue => rollover + 1
            case num => num
          }
        }
      }
    }

    helper(expandGrid(grid))
  }

  val grid = io.Source.fromResource("2021/day15.txt").getLines.map(_.toCharArray.map(_.asDigit)).toArray

  println(solution1(grid))
  println(solution2(grid))
}
