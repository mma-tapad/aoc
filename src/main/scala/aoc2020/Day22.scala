package aoc2020

import scala.annotation.tailrec

object Day22 extends App {
  def scoreDeck(deck: List[Int]): Int = deck
    .reverse
    .zipWithIndex
    .foldLeft(0) { case (sum, (card, index)) =>
      sum + (card * (index+1))
    }

  def solution1(deck1: List[Int], deck2: List[Int]): Int = {
    @tailrec
    def play(deck1: List[Int], deck2: List[Int]): List[Int] = {
      (deck1, deck2) match {
        case (Nil, d2) => d2
        case (d1, Nil) => d1
        case (head1 :: tail1, head2 :: tail2) =>
          val toAppend: List[Int] = List(head1, head2).sorted.reverse
          if(head1 > head2) play(tail1 ++ toAppend, tail2)
          else play(tail1, tail2 ++ toAppend)
      }
    }

    scoreDeck(play(deck1, deck2))
  }

  def solution2(deck1: List[Int], deck2: List[Int]) = {
    def play(
      deck1: List[Int],
      deck2: List[Int],
      gameMemory: Set[(List[Int], List[Int])] = Set.empty
    ): (List[Int], Boolean) = {
      if(gameMemory.contains(deck1 -> deck2)) deck1 -> true
      else {
        val newMem = gameMemory incl (deck1, deck2)
        (deck1, deck2) match {
          case (Nil, d2) => d2 -> false
          case (d1, Nil) => d1 -> true

          case (head1 :: tail1, head2 :: tail2) if tail1.size >= head1 & tail2.size >= head2 =>
            val (_, p1Win) = play(tail1.take(head1), tail2.take(head2), gameMemory)
            val toAppend: List[Int] = List(head1, head2)
            if(p1Win) play(tail1 ++ toAppend, tail2, newMem)
            else play(tail1, tail2 ++ toAppend.reverse, newMem)

          case (head1 :: tail1, head2 :: tail2) =>
            val toAppend: List[Int] = List(head1, head2).sorted.reverse
            if(head1 > head2) play(tail1 ++ toAppend, tail2, newMem)
            else play(tail1, tail2 ++ toAppend, newMem)
        }
      }
    }

    scoreDeck(play(deck1, deck2)._1)
  }

  val (deck1, deck2) = {
    val regex = """Player \d: (\d+\s*)+""".r

    val line = io.Source.fromResource("2020/day22.txt")
      .getLines
      .mkString(" ")

    regex
      .findAllMatchIn(line)
      .toSeq
      .map {
        _
          .group(0)
          .drop("Player 1: ".size)
          .trim
          .split(" ")
          .toList
          .map(_.toInt)
      }
      .take(2) match {
        case Seq(a, b) => a -> b
      }
  }

  println(solution1(deck1, deck2))
  println(solution2(deck1, deck2))
}
