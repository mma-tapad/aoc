package aoc

object Day06 extends App {

  def solution1(answers: Seq[String]): Int = {
    answers.foldLeft(0) { case (acc, answer) =>
      acc + answer.replaceAll("\\s+", "").toCharArray.toSet.size
    }
  }

  def solution2(answers: Seq[String]): Int = {
    answers.foldLeft(0) { case (acc, answer) =>
      val charSets = answer.split(" ").map(_.toCharArray.toSet)
      var head = charSets.head
      charSets.tail.foreach { set =>
        head = head intersect set
      }

      acc + head.size
    }
//    answers.foldLeft(0) { case (acc, answer) =>
//      acc +
//        answer
//          .split(" ")
//          .map(buildBits)
//          .foldLeft("1" * 26)(bitwiseAnd)
//          .count(_ == '1')
//    }
  }

  //TODO: Figure out how to bits in scala
  def buildBits(str: String) = {
    val alphabet = IndexedSeq('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')

    val res = alphabet
      .indices
      .map { index =>
        if(str.length <= index || str.charAt(index) != alphabet(index)) '0'
        else '1'
      }
      .foldLeft("")(_ + _)
    println(res)
    res
  }

  def bitwiseAnd(str1: String, str2: String): String = {
    str1.indices.foldLeft("") { case (acc, index) =>
      if(str1.charAt(index) == '1' && str2.charAt(index) == '1') acc :+ '1'
      else acc :+ '0'
    }
  }

  val answers = Utils.consumeText(io.Source.fromResource("day06.txt").getLines.toSeq)

  println(solution1(answers))
  println(solution2(answers))
}
