package aoc2020

import scala.collection.mutable

object Day09 extends App {
  def solution1(numbers: Seq[Long]): Long = {
    val queue = new FiniteQueueMap[Long](25)

    numbers.take(25).foreach(n => queue.enqueue(n))

    numbers.drop(25).find { n =>
      val found = queue.hashMap.keys.forall { key =>
        val diff = n - key
        !queue.hashMap.contains(diff) ||
          (diff == key && queue.hashMap(key) <= 1)
      }
      if(!found) {
        queue.enqueue(n)
      }
      found
    }.get
  }

  def solution2(numbers: Seq[Long], sum: Long): Long = {
    val queue = mutable.Queue.empty[Long]
    var acc = 0L

    numbers.find { n =>
      var potential = acc + n
      while(potential > sum) {
        potential = potential - queue.dequeue()
      }
      if(potential == sum) {
        true
      } else {
        acc = potential
        queue.enqueue(n)
        false
      }
    }

    val sorted = queue.sorted
    sorted.head + sorted.last
  }

  val numbers = io.Source.fromResource("2020/day09.txt").getLines.toSeq.map(_.toLong)

  val sol1 = solution1(numbers)

  println(sol1)
  println(solution2(numbers, sol1))
}

class FiniteQueueMap[A](limit: Int) extends mutable.Queue[A] {
  val hashMap: mutable.Map[A, Int] = mutable.Map.empty[A, Int].withDefaultValue(0)

  override def enqueue(elem: A): FiniteQueueMap.this.type = {
    this += elem
    mapIncr(elem)
    while(super.size > limit) {
      val key = super.dequeue()
      mapDecr(key)
    }
    this
  }

  private def mapIncr(elem: A): Unit = {
    hashMap.put(elem, hashMap(elem) + 1)
  }

  private def mapDecr(elem: A): Unit = {
    val value = hashMap(elem)
    if(value <= 1) {
      hashMap.remove(elem)
    } else {
      hashMap.put(elem, value - 1)
    }
  }
}