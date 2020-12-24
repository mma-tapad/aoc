package aoc

import scala.annotation.tailrec

object Day23 extends App {
  @tailrec
  def play(cups: Node[Int], nodeMap: Map[Int, Node[Int]], numTimes: Int = 100): Node[Int] = {
    numTimes match {
      case 0 => cups
      case _ =>
        val currCup = cups
        val (_, firstRemoved) = cups.remove(1, 3)

        val (removedThree, _) = (0 until 3)
          .foldLeft(Seq.empty[Int], firstRemoved) { case ((acc, node), _) =>
            (acc appended node.value, node.next)
          }

        val lastRemoved = firstRemoved(2)

        val maxValue = removedThree.sorted.reverse.foldLeft(nodeMap.size) { case (currMax, next) =>
          if(next == currMax) currMax - 1 else currMax
        }

        val targetCup: Node[Int] = {
          val currLabel = if (currCup.value == 1) maxValue else currCup.value - 1

          val targetValue = removedThree
            .indices
            .foldLeft(currLabel) { case (cl, _) =>
              if (removedThree.contains(cl)) {
                if (cl == 1) maxValue
                else cl - 1
              } else cl
            }

          nodeMap(targetValue)
        }

        targetCup.append(firstRemoved, lastRemoved)

        play(cups.next, nodeMap, numTimes-1)
    }
  }

  def solution1(cups: Array[Int]): String = {
    val nodes = Node.from(cups)
    val nodeMap = nodes.buildNodeMap(cups.length)

    play(nodes, nodeMap)
    val oneCup = nodeMap(1)

    (0 until cups.length-1).foldLeft("", oneCup) { case ((acc, node), _) =>
      (acc + node.next.value) -> node.next
    }._1
  }

  def solution2(cups: Array[Int]): Long = {
    val nodes = Node.from(cups)
    val nodeMap = nodes.buildNodeMap(cups.length)

    play(nodes, nodeMap, 10000000)
    val oneCup = nodeMap(1)

    (0 until 2).foldLeft(1L, oneCup) { case ((acc, node), _) =>
      (acc * node.next.value, node.next)
    }._1
  }

  val cups = io.Source.fromResource("day23.txt").getLines.next().map(_.toString.toInt).toArray
  val cups2 = cups ++ (10 to 1000000)

  println(solution1(cups))
  println(solution2(cups2))
}

case class Node[T](value: T) {
  var next: Node[T] = _

  def apply(idx: Int): Node[T] = (0 until idx).foldLeft(this) { case (node, _) =>
    node.next
  }

  def remove(removeIndex: Int, numRemoved: Int): (Node[T], Node[T]) = {
    val firstRemoved = this(removeIndex)
    this.next = firstRemoved(numRemoved)
    this -> firstRemoved
  }

  def append(head: Node[T], last: Node[T]): Node[T] = {
    val next = this.next
    this.next = head
    last.next = next
    this
  }

  def buildNodeMap(iterations: Int): Map[T, Node[T]] = {
    (0 until iterations).foldLeft(Map.empty[T, Node[T]], this) { case ((acc, node), _) =>
      acc.updated(node.value, node) -> node.next
    }._1
  }
}

object Node {
  def from[T](coll: Iterable[T]): Node[T] = {
    val nodes = coll.map(Node(_))

    nodes
      .sliding(2)
      .foreach { a =>
        a.head.next = a.last
      }

    nodes.last.next = nodes.head
    nodes.head
  }
}



