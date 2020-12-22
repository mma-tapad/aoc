package aoc

object Day19 extends App {
  val letterRegex = """(\d+): "(\w)"""".r
  val singleRuleRegex = """(\d+): (\d+)""".r
  val doubleRuleRegex = """(\d+): (\d+) (\d+)""".r
  val branchSingleRuleRegex = """(\d+): (\d+) \| (\d+)""".r
  val branchDoubleRuleRegex = """(\d+): (\d+) (\d+) \| (\d+) (\d+)""".r

  def solution1(rules: Seq[String]): Int = {
    def buildRegex(ruleMap: Map[Int, MessageRule], rule: MessageRule, validString: String = ""): String = {
      rule match {
        case LetterRule(c) =>
          validString + c
        case SingleRule(index) =>
          buildRegex(ruleMap, ruleMap(index), validString)
        case DoubleRule(index1, index2) =>
          buildRegex(ruleMap, ruleMap(index1), validString) + buildRegex(ruleMap, ruleMap(index2))
        case BranchRule(r1, r2) =>
          validString + s"(${buildRegex(ruleMap, r1)}|${buildRegex(ruleMap, r2)})"
        case RepeatingSingleRule(index) =>
          validString + s"(${buildRegex(ruleMap, ruleMap(index))})+"
        case RepeatingDoubleRule(index1, index2) =>
          validString + s"(${buildRegex(ruleMap, ruleMap(index1))})+?(${buildRegex(ruleMap, ruleMap(index2))})+?"
      }
    }

    val ruleMap = rules
      .foldLeft(Map.empty[Int, MessageRule]) { case (ruleMap, rule) =>
        rule match {
          case "8: 42 | 42 8" =>
            ruleMap.updated(8, RepeatingSingleRule(42))
          case "11: 42 31 | 42 11 31" =>
            ruleMap.updated(11, RepeatingDoubleRule(42, 31))
          case letterRegex(id, letter) =>
            ruleMap.updated(id.toInt, LetterRule(letter(0)))
          case singleRuleRegex(id, rule1) =>
            ruleMap.updated(id.toInt, SingleRule(rule1.toInt))
          case doubleRuleRegex(id, rule1, rule2) =>
            ruleMap.updated(id.toInt, DoubleRule(rule1.toInt, rule2.toInt))
          case branchSingleRuleRegex(id, rule1, rule2) =>
            ruleMap.updated(id.toInt, BranchRule(SingleRule(rule1.toInt), SingleRule(rule2.toInt)))
          case branchDoubleRuleRegex(id, rule1, rule2, rule3, rule4) =>
            ruleMap.updated(id.toInt, BranchRule(DoubleRule(rule1.toInt, rule2.toInt), DoubleRule(rule3.toInt, rule4.toInt)))
          case _ =>
            ruleMap
        }
      }

    val regex = buildRegex(ruleMap, ruleMap(0))
    val regexHacks = (1 to 10).map { i =>
      regex.replace("+?", s"{$i}").r
    }

    rules.count(r => regexHacks.exists(_.matches(r)))
  }

  val rules1 = io.Source.fromResource("day19.txt").getLines.toSeq
  val rules2 = rules1.map {
    case "8: 42" => "8: 42 | 42 8"
    case "11: 42 31" => "11: 42 31 | 42 11 31"
    case r => r
  }


  println(solution1(rules1))
  println(solution1(rules2))
}

sealed trait MessageRule

case class LetterRule(c: Char) extends MessageRule
case class SingleRule(rule: Int) extends MessageRule
case class DoubleRule(rule1: Int, rule2: Int) extends MessageRule
case class BranchRule(rule1: MessageRule, rule2: MessageRule) extends MessageRule
case class RepeatingSingleRule(rule: Int) extends MessageRule
case class RepeatingDoubleRule(rule1: Int, rule2: Int) extends MessageRule