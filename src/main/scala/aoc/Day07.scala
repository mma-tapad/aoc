package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day07 extends App {
  val coloredBagRegex = "(^|\\d+\\s)((\\w+\\s)+?)bag".r

  def addOrUpdate[K, V](map: mutable.Map[K, List[V]], key: K, value: V): Unit = {
    map.get(key) match {
      case Some(v) => map.update(key, v :+ value)
      case _       => map.put(key, value :: Nil)
    }
  }

  def solution1(bagRules: Seq[String]): Int = {
    def traverse[K](map: mutable.Map[K, List[K]], key: K, acc: List[K] = Nil)
    : List[K] = {
      map.get(key) match {
        case None => acc
        case Some(list) => list.flatMap { value =>
          traverse(map, value, acc :+ value)
        }
      }
    }

    val bagMap = mutable.Map[String, List[String]]()
    bagRules.foreach { rule =>
      val matches = coloredBagRegex.findAllMatchIn(rule).toSeq
      matches.headOption.fold({}) { m =>
        val parent = m.group(2).trim
        val children = matches.tail.map(_.group(2).trim)
        children.foreach { child =>
          addOrUpdate(bagMap, child, parent)
        }
      }
    }
    traverse(bagMap, "shiny gold").toSet.size
  }

  def solution2(bagRules: Seq[String]): Int = {
    def traverse2(map: mutable.Map[String, List[(String, Int)]], key: String, sum: Int = 0)
    : Int = {
      map.get(key) match {
        case None => sum
        case Some(list) => list.foldLeft(sum + list.map(_._2).sum) { case (acc, value) =>
          acc + (value._2 * traverse2(map, value._1))
        }
      }
    }

    val bagMap = mutable.Map[String, List[(String, Int)]]()
    bagRules.foreach { rule =>
      val matches = coloredBagRegex.findAllMatchIn(rule).toSeq
      matches.headOption.fold({}) { m =>
        val parent: String = m.group(2).trim
        val children: Seq[(String, Int)] = matches.tail.map { m =>
          m.group(2).trim -> m.group(1).trim.toInt
        }
        children.foreach { child =>
          addOrUpdate(bagMap, parent, child)
        }
      }
    }
    traverse2(bagMap, "shiny gold")
  }

  val bagRules = io.Source.fromResource("day07.txt").getLines.toSeq

  println(solution1(bagRules))
  println(solution2(bagRules))
}
