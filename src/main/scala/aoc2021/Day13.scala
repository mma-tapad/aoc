package aoc2021

import aoc2021.Utils.Point

object Day13 extends App {
  def parseInstructions(lines: List[String]): (Set[Point], List[FoldInstruction]) = {
    val (firstInstructions, secondInstructions) = lines.partition(!_.contains("fold"))

    firstInstructions.map {
      case s"$x,$y" => Point(x.toInt, y.toInt)
    }.toSet ->
      secondInstructions.map {
        case s"fold along y=$line" => FoldInstruction(YAxis, line.toInt)
        case s"fold along x=$line" => FoldInstruction(XAxis, line.toInt)
      }
  }

  def solution1(lines: List[String]): Int = {
    val (points, folds) = parseInstructions(lines)

    folds
      .head
      .apply(points)
      .size
  }

  def solution2(lines: List[String]): String = {
    def toPrettyPrintList(points: Set[Point]): List[String] = {
      val maxX = points.maxBy(_.x).x
      val maxY = points.maxBy(_.y).y
      val pointsByY: Map[Int, Set[Point]] = points.groupBy(_.y)

      (0 to maxY).map { y =>
        val linePoints = pointsByY.getOrElse(y, Set.empty)
        (0 to maxX).foldLeft("") { case (str, x) =>
          val char = if(linePoints.contains(Point(x,y))) {
            "#"
          } else {
            "."
          }
          str + char
        }
      }.toList
    }

    val (points, folds) = parseInstructions(lines)
    val finalPoints = folds
      .foldLeft(points) { case (acc, fold) =>
        fold.apply(acc)
      }

    toPrettyPrintList(finalPoints).mkString("\n")
  }

  val lines = io.Source.fromResource("2021/day13.txt").getLines.toList.filterNot(_.isEmpty)

  println(solution1(lines))
  println(solution2(lines))
}

sealed trait Axis {
  def pointField: Point => Int
  def copyMethod: (Point, Int) => Point
}
case object XAxis extends Axis {
  override def pointField: Point => Int = _.x
  override def copyMethod: (Point, Int) => Point = { case (pt, target) =>
    pt.copy(x = target)
  }
}
case object YAxis extends Axis {
  override def pointField: Point => Int = _.y
  override def copyMethod: (Point, Int) => Point = { case (pt, target) =>
    pt.copy(y = target)
  }
}
case class FoldInstruction(axis: Axis, line: Int) {
  def apply(points: Set[Point]) : Set[Point]= {
    val toFolds = points.filter(pt => axis.pointField(pt) > line)
    val foldeds = toFolds.map(pt => axis.copyMethod(pt, axis.pointField(pt) - ((axis.pointField(pt) - line) * 2)))

    points -- toFolds ++ foldeds
  }
}
