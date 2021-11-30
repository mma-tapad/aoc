package aoc2020

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Day11 extends App {
  val directions: Seq[(Int => Int, Int => Int)] = {
    val functions = Seq[Int => Int](identity, _ + 1, _ - 1)
    //cross product for every adjacent direction
    (for {
      x <- functions
      y <- functions
    } yield
      x -> y
    ).tail // remove the (identity, identity) pair
  }

  @tailrec
  def applySeatRules(
    seatingChart: Seq[Seq[Char]],
    tolerance: Int,
    chartAcc: Set[Seq[Seq[Char]]] = Set.empty[Seq[Seq[Char]]]
  )(readOccupancyFn: Seq[Seq[Char]] => (Int, Int) => (Int => Int, Int => Int) => Int): Seq[Seq[Char]] = {
    val newChart = seatingChart
      .map(_.zipWithIndex)
      .zipWithIndex
      .map { case (row, y) =>
        row.map { case (seat, x) =>
          seat match {
            case '.' => seat
            case _ =>
              val numAdjSeats = directions
                .foldLeft(0) { case (acc, (xDir, yDir)) =>
                  acc + readOccupancyFn(seatingChart)(xDir(x), yDir(y))(xDir, yDir)
                }

              if(numAdjSeats == 0) '#'
              else if(numAdjSeats >= tolerance) 'L'
              else seat
          }
        }
      }

    if(chartAcc.contains(newChart)) newChart
    else applySeatRules(newChart, tolerance, chartAcc + newChart)(readOccupancyFn)
  }

  def solution1(seatingChart: Seq[Seq[Char]]): Int = {
    def readSeatOccupancy(chart: Seq[Seq[Char]])
      (x: Int, y: Int)
      (xDir: Int => Int, yDir: Int => Int): Int = {
      Try(chart(y)(x)) match {
        case Success('#') => 1
        case _ => 0
      }
    }

    applySeatRules(seatingChart, 4)(readSeatOccupancy)
      .map { row =>
        row.count(_ == '#')
      }
      .sum
  }

  def solution2(seatingChart: Seq[Seq[Char]]): Int = {
    @tailrec
    def readSeatOccupancy(rows: Seq[Seq[Char]])
      (x: Int, y: Int)
      (xDir: Int => Int, yDir: Int => Int): Int = {
      Try(rows(y)(x)) match {
        case Success('#') => 1
        case Success('L') => 0
        case Failure(_) => 0
        case _ => readSeatOccupancy(rows)(xDir(x),  yDir(y))(xDir, yDir)
      }
    }

    applySeatRules(seatingChart, 5)(readSeatOccupancy)
      .map { row =>
        row.count(_ == '#')
      }
      .sum
  }

  val seatingChart = io.Source.fromResource("2020/day11.txt").getLines.toSeq.map(_.toSeq)

  println(solution1(seatingChart))
  println(solution2(seatingChart))
}
