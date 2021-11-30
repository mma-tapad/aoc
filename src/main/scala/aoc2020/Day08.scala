package aoc2020

import scala.collection.mutable

object Day08 extends App {
  val instructionRegex = "(\\w+)\\s([+-])(\\d+)".r

  def processInstruction(instruction: String) = {
    instructionRegex.findFirstMatchIn(instruction).map { m =>
      Instruction(
        operator = m.group(1) match {
          case "acc" => Acc
          case "jmp" => Jmp
          case _     => Nop
        },
        isPositive = m.group(2) == "+",
        amount = m.group(3).toInt
      )
    }.get
  }

  def solution1(instructions: IndexedSeq[String]): Int = {
    val visitedLines = mutable.Set.empty[Int]
    var acc = 0
    var lineIndex = 0
    while(!visitedLines.contains(lineIndex)) {
      visitedLines.add(lineIndex)
      processInstruction(instructions(lineIndex)) match {
        case Instruction(op, isPos, amt) =>
          val change = if(isPos) amt else amt * -1
          op match {
            case Acc => acc = acc + change
            case Jmp => lineIndex = lineIndex + change - 1
            case Nop =>
        }
      }
      lineIndex = lineIndex + 1
    }
    acc
  }

  def solution2(instructions: IndexedSeq[String]): Int = {
    val breakpoints: Seq[Int] = instructions
      .zipWithIndex
      .flatMap { case (inst, index) =>
        if(inst.contains("jmp") || inst.contains("nop")) Some(index)
        else None
      }

    breakpoints.flatMap { bpIndex =>
      val visitedLines = mutable.Set.empty[Int]
      var acc = 0
      var lineIndex = 0
      var looping = false

      while(lineIndex < instructions.size && !looping) {
        if(visitedLines.contains(lineIndex)) {
          looping = true
        } else {
          visitedLines.add(lineIndex)
          val currInstruction = if(lineIndex == bpIndex) {
            Instruction.flip(processInstruction(instructions(lineIndex)))
          } else {
            processInstruction(instructions(lineIndex))
          }

          currInstruction match {
            case Instruction(op, isPos, amt) =>
              val change = if(isPos) amt else amt * -1
              op match {
                case Acc => acc = acc + change
                case Jmp => lineIndex = lineIndex + change - 1
                case Nop =>
              }
          }
          lineIndex = lineIndex + 1
        }
      }
      if(looping) None
      else Some(acc)
    }.head
  }


  val instructions = io.Source.fromResource("2020/day08.txt").getLines.toIndexedSeq

  println(solution1(instructions))
  println(solution2(instructions))
}

sealed trait Operator
case object Acc extends Operator
case object Jmp extends Operator
case object Nop extends Operator

case class Instruction(operator: Operator, isPositive: Boolean, amount: Int) {
  override def toString: String = s"${operator.toString.toLowerCase} ${if(isPositive) "+" else "-"}$amount"
}
object Instruction {
  def unapply(instruction: Instruction): Option[(Operator, Boolean, Int)] = {
    Option((instruction.operator, instruction.isPositive, instruction.amount))
  }
  def flip(instruction: Instruction): Instruction = {
    instruction.copy(
      operator = instruction.operator match {
        case Jmp => Nop
        case Nop => Jmp
        case Acc => Acc
      }
    )
  }
}
