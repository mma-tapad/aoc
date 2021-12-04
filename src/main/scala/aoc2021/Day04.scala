package aoc2021

object Day04 extends App {

  def solution1(boards: List[Board], numbers: Array[Int]) = {
    numbers.foldLeft(-1) {
      case (-1, number) =>
        val updated = boards.map(Board.removeNumber(_, number))

        updated.find(_.hasWon) match {
          case Some(board) =>
            board.sum * number
          case _ => -1
        }

      case (answer, _) => answer
    }
  }

  def solution2(boards: List[Board], numbers: Array[Int]) = {
    val (_, winningBoard, winningNumber) = numbers.foldLeft((boards, Board.Empty, -1)) { case ((updatedBoards, recentWinner, recentWinningNumber), number) =>
      val updated = updatedBoards.map(Board.removeNumber(_, number))
      val (winners, losers) = updated.partition(_.hasWon)

      winners.headOption match {
        case Some(winner) => (losers, winner, number)
        case None => (losers, recentWinner, recentWinningNumber)
      }
    }

    winningBoard.sum * winningNumber
  }

  val lines = io.Source.fromResource("2021/day04.txt").getLines.toList
  val numbers: Array[Int] = lines.head.split(",").map(_.toInt)
  val boards: List[Board] =
    lines
      .drop(2)
      .filterNot(_.isEmpty)
      .sliding(5, 5)
      .toList
      .map { board =>
        val parsedBoard = board.toArray
          .map { row =>
            row.trim.split("\\s+")
              .map{ num =>
                Option(num.toInt)
              }
          }
        Board(parsedBoard)
      }

  println(solution1(boards, numbers))
  println(solution2(boards, numbers))
}

case class Board(private val inner: Array[Array[Option[Int]]]) {
  import Board._
  val rows: Seq[Array[Option[Int]]] = inner.toList
  def cols: Seq[Array[Option[Int]]] = {
    inner.indices.map { x =>
      inner.indices.map { y =>
        inner(y)(x)
      }
      .toArray
    }
    .toList
  }

  def hasWon: Boolean = {
    rows.exists(_ sameElements EmptyArray) || cols.exists(_ sameElements EmptyArray)
  }

  def sum: Int = inner.flatMap(_.flatten).sum
}

object Board {
  val EmptyArray: Array[Option[Int]] = Array.fill[Option[Int]](5)(None)
  val Empty: Board = Board(Array.empty[Array[Option[Int]]])

  def removeNumber(board: Board, number: Int): Board = {
    for {
      x <- board.inner.indices
      y <- board.inner.indices
    } {
      if(board.inner(x)(y).contains(number)) {
        board.inner(x)(y) = None
      }
    }

    board
  }
}