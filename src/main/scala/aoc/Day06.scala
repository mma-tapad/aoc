package aoc

object Day06 extends App {

  def solution1(answers: Seq[String]): Int = {
    answers.foldLeft(0) { case (acc, answer) =>
      acc + answer.replaceAll("\\s+", "").toCharArray.toSet.size
    }
  }

  def solution2(answers: Seq[String]): Int = {
    /* Set intersection approach
    answers.foldLeft(0) { case (acc, answer) =>
      val charSets = answer.split(" ").map(_.toCharArray.toSet)
      var head = charSets.head
      charSets.tail.foreach { set =>
        head = head intersect set
      }

      acc + head.size
    }
    */
    
    // BitSeq approach
    answers.foldLeft(0) { case (acc, answer) =>
      acc +
        answer
          .split(" ")
          .map(buildBits)
          .foldLeft(BitSeq("1" * 26))(_ & _)
          .cardinality
    }
  }

  def buildBits(str: String): BitSeq = {
    val availableChars = str.map(_ - 97).toSet
    val bitString = (0 to 25).foldLeft("") { case (acc, index) =>
      if(availableChars.contains(index)) acc + '1'
      else acc + '0'
    }

    BitSeq(bitString)
  }

  val answers = Utils.consumeText(io.Source.fromResource("day06.txt").getLines.toSeq)

  case class BitSeq private(underlying: IndexedSeq[Boolean]) {
    def &(other: BitSeq): BitSeq = {
      val result = underlying.zip(other.underlying).map { case (thisB, thatB) =>
        thisB && thatB
      }
      BitSeq(result)
    }

    def cardinality: Int = underlying.count(identity)
  }

  object BitSeq {
    def apply(bitString: String): BitSeq = {
      val underlying = bitString.map(_ == '1')
      BitSeq(underlying)
    }
  }

  println(solution1(answers))
  println(solution2(answers))
}
