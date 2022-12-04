package aoc2022

object Day02 extends App {

  // Find who wins given two RPS moves. 6pts for win, 3 for draw, 0 for loss.
  // Also, 3pts for playing Scissors, 2pts for Paper, 1pt for Rock.
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

  // Find the correct response move given the goal provided. Same scoring as above.
  def solution2(moves: List[(Char, Char)]): Int = {
    moves.map { case (first, second) =>
      val (oppMove, goal) = Move(first) -> Goal(second)

      goal.score + (goal match {
        case Win  => oppMove.beatenBy.score
        case Lose => oppMove.beats.score
        case _    => oppMove.score
      })
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
  val score: Int
}
case object Rock extends Move {
  def beats: Move = Scissors
  def beatenBy: Move = Paper
  val score = 1
}
case object Paper extends Move {
  def beats: Move = Rock
  def beatenBy: Move = Scissors
  val score = 2
}
case object Scissors extends Move {
  def beats: Move = Paper
  def beatenBy: Move = Rock
  val score = 3
}

object Move {
  def apply(c: Char): Move = c match {
    case 'A' | 'X' => Rock
    case 'B' | 'Y' => Paper
    case _         => Scissors
  }
}

sealed trait Goal {
  val score: Int
}
case object Win extends Goal {
  val score = 6
}
case object Lose extends Goal {
  val score = 0
}
case object Draw extends Goal {
  val score = 3
}

object Goal {
  def apply(c: Char): Goal = c match {
    case 'X' => Lose
    case 'Y' => Draw
    case _   => Win
  }
}
