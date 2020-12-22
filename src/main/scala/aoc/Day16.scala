package aoc

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.matching.Regex

object Day16 extends App {

  def solution1(rules: Seq[Rule], nearbyTickets: Seq[Seq[Int]]): Int = {
    nearbyTickets.foldLeft(0) { case (acc, ticket) =>
      acc + ticket.filter(value => !rules.exists(_(value))).sum
    }
  }

  def solution2(rules: Seq[Rule], myTicket: Seq[Int], nearbyTickets: Seq[Seq[Int]]) = {
    val validNearbyTickets: Seq[Seq[Int]] = nearbyTickets
      .filter { ticket =>
        ticket.forall { value =>
          rules.exists(_(value))
        }
      }

    val valuesByColumn: Map[Int, Seq[Int]] = rules.indices
      .map { index =>
        index -> validNearbyTickets.map(_(index))
      }
      .toMap

    val ruleToValidColumns: Map[Rule, Set[Int]] = rules.map { rule =>
      rule -> valuesByColumn.collect {
        case (col, values) if values.forall(v => rule(v)) => col
      }.toSet
    }.toMap

    @tailrec
    def findValidColumn(ruleMap: Map[Rule, Set[Int]], acc: Map[Rule, Int] = Map.empty[Rule, Int]): Map[Rule, Int] = {
      ruleMap
        .find { case (_, cols) =>
          cols.size == 1
        } match {
        case Some((rule, col)) =>
          val validColumn = col.head
          val updatedMap = ruleMap.view.mapValues(_.excl(validColumn)).toMap
          findValidColumn(updatedMap, acc.updated(rule, validColumn))
        case _ =>
          acc
      }
    }

    val ruleToValidColumn: Map[Rule, Int] = findValidColumn(ruleToValidColumns)

    ruleToValidColumn
      .view
      .filterKeys(_.label.startsWith("departure"))
      .foldLeft(1L) { case (acc, (_, col)) =>
        acc * myTicket(col)
      }
  }

  val tickets = io.Source.fromResource("day16.txt").getLines.toSeq

  val (rules, myTicket, nearbyTickets, _) = tickets
    .foldLeft((Seq.empty[Rule], Seq.empty[Int], Seq.empty[Seq[Int]], true)) { case ((rules, myTicket, nearbyTickets, readingMyTicket), ticket) =>
      ticket.trim match {
        case Rule.regex(label, low, low2, high, high2) => (rules.appended(Rule(label, low, low2, high, high2)), myTicket, nearbyTickets, readingMyTicket)
        case "your ticket:" => (rules, myTicket, nearbyTickets, true)
        case "nearby tickets:" => (rules, myTicket, nearbyTickets, false)
        case "" => (rules, myTicket, nearbyTickets, readingMyTicket)
        case line if readingMyTicket => (rules, line.split(",").toSeq.map(_.toInt), nearbyTickets, readingMyTicket)
        case line if !readingMyTicket => (rules, myTicket, nearbyTickets.appended(line.split(",").toSeq.map(_.toInt)), readingMyTicket)
      }
    }

  println(solution1(rules, nearbyTickets))
  println(solution2(rules, myTicket, nearbyTickets))
}

case class Rule(label: String, lowerRange: Range, higherRange: Range) {
  def apply(value: Int): Boolean = lowerRange.contains(value) || higherRange.contains(value)
}

object Rule {
  val regex: Regex = """^(\w+\s?\w+): (\d+)-(\d+) or (\d+)-(\d+)$""".r

  def apply(label: String, low: String, low2: String, high: String, high2: String): Rule = Rule(
    label, low.toInt to low2.toInt, high.toInt to high2.toInt
  )
}
