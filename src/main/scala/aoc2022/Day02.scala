package aoc2022

object Day02 extends App {
  def solution1(moves: List[(Char, Char)]): Int = {
    moves.map { case (first, second) =>
      val (oppMove, myMove) = Move(first) -> Move(second)

      myMove.score + (myMove match {
        case m if m == oppMove.beats    => 0
        case m if m == oppMove.beatenBy => 6
        case _                          => 3
      })
    }
    .sum
  }


  def solution2(moves: List[(Char, Char)]): Int = {
    moves.map { case (first, second) =>
      val (oppMove, goal) = Move(first) -> Goal(second)
      goal match {
        case Win  => 6 + oppMove.beatenBy.score
        case Lose => oppMove.beats.score
        case _    => 3 + oppMove.score
      }
    }
    .sum
  }

  val moves = io.Source.fromResource("2022/day02.txt")
    .getLines
    .map { line =>
      val arr = line.split(" ")
      arr.head.head -> arr.last.head
    }
    .toList

  println(solution1(moves))
  println(solution2(moves))
}

sealed trait Move {
  def beats: Move
  def beatenBy: Move
  def score: Int
}
case object Rock extends Move {
  def beats: Move = Scissors
  def beatenBy: Move = Paper
  def score = 1
}
case object Paper extends Move {
  def beats: Move = Rock
  def beatenBy: Move = Scissors
  def score = 2
}
case object Scissors extends Move {
  def beats: Move = Paper
  def beatenBy: Move = Rock
  def score = 3
}

object Move {
  def apply(c: Char): Move = c match {
    case 'A' | 'X' => Rock
    case 'B' | 'Y' => Paper
    case _         => Scissors
  }
}

sealed trait Goal
case object Win extends Goal
case object Lose extends Goal
case object Draw extends Goal

object Goal {
  def apply(c: Char): Goal = c match {
    case 'X' => Lose
    case 'Y' => Draw
    case _   => Win
  }
}
