package aoc

import scala.annotation.tailrec

sealed trait Orientation {
  def turnRight: Orientation
  def turnLeft: Orientation
  def toCommand: String
}

case object North extends Orientation {
  override def turnRight: Orientation = East
  override def turnLeft: Orientation = West
  override def toCommand: String = "N"
}
case object East extends Orientation {
  override def turnRight: Orientation = South
  override def turnLeft: Orientation = North
  override def toCommand: String = "E"
}
case object South extends Orientation {
  override def turnRight: Orientation = West
  override def turnLeft: Orientation = East
  override def toCommand: String = "S"
}
case object West extends Orientation {
  override def turnRight: Orientation = North
  override def turnLeft: Orientation = South
  override def toCommand: String = "W"
}

case class Coord(x: Int, y: Int) {
  def turnRight: Coord = Coord(y, x * -1)
  def turnLeft: Coord = Coord(y * -1, x)

  def +(that: Coord): Coord = Coord(x + that.x, y + that.y)
  def multiMove(distance: Int): Coord = Coord(x * distance, y * distance)

  def manhattanDistance: Int = Math.abs(x) + Math.abs(y)
}

object Coord {
  val DEFAULT: Coord = Coord(0, 0)
}

object Day12 extends App {
  val regex = "(\\w)(\\d+)".r
  def parseDirection(direction: String): Option[(String, Int)] = {
    regex.findFirstMatchIn(direction).map { m =>
      m.group(1) -> m.group(2).toInt
    }
  }

  def turn[T](numTimes: Int, start: T, direction: T => T): T = {
    (0 until numTimes)
      .foldLeft(start) { case (c, _) =>
        direction(c)
      }
  }

  def solution1(directions: Seq[(String, Int)]): Int = {
    @tailrec
    def executeCommand(currOrientation: Orientation, command: String, distance: Int): (Orientation, Coord) = {
      command match {
        case "N" => currOrientation                                                -> Coord(0, distance)
        case "S" => currOrientation                                                -> Coord(0, distance * -1)
        case "E" => currOrientation                                                -> Coord(distance, 0)
        case "W" => currOrientation                                                -> Coord(distance * -1, 0)
        case "R" => turn[Orientation](distance / 90, currOrientation, _.turnRight) -> Coord.DEFAULT
        case "L" => turn[Orientation](distance / 90, currOrientation, _.turnLeft)  -> Coord.DEFAULT
        case _   => executeCommand(currOrientation, currOrientation.toCommand, distance)
      }
    }

    val (destination, _) = directions
      .foldLeft((Coord.DEFAULT, East: Orientation)) { case ((coord, currOrientation), (command, distance)) =>
        val (newOrientation, coordMovement) = executeCommand(currOrientation, command, distance)
        (coord + coordMovement, newOrientation)
      }

    destination.manhattanDistance
  }

  def solution2(directions: Seq[(String, Int)]): Int = {
    def executeCommand(wayPointCoord: Coord, command: String, distance: Int): (Coord, Coord) = {
      command match {
        case "N" => Coord(wayPointCoord.x, wayPointCoord.y + distance)     -> Coord.DEFAULT
        case "S" => Coord(wayPointCoord.x, wayPointCoord.y - distance)     -> Coord.DEFAULT
        case "E" => Coord(wayPointCoord.x + distance, wayPointCoord.y)     -> Coord.DEFAULT
        case "W" => Coord(wayPointCoord.x - distance, wayPointCoord.y)     -> Coord.DEFAULT
        case "R" => turn[Coord](distance / 90, wayPointCoord, _.turnRight) -> Coord.DEFAULT
        case "L" => turn[Coord](distance / 90, wayPointCoord, _.turnLeft)  -> Coord.DEFAULT
        case _   => wayPointCoord                                          -> wayPointCoord.multiMove(distance)
      }
    }

    val (_, destination) = directions
      .foldLeft((Coord(10, 1), Coord.DEFAULT)) { case ((relativeWaypointCooord, shipCoord), (command, distance)) =>

      val (newRelativeWaypointCoord, shipMovement) = executeCommand(relativeWaypointCooord, command, distance)
      (newRelativeWaypointCoord, shipCoord + shipMovement)
    }

    destination.manhattanDistance
  }
  val directions = io.Source.fromResource("day12.txt").getLines.toSeq.flatMap(parseDirection)

  println(solution1(directions))
  println(solution2(directions))
}