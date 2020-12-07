package aoc

import scala.annotation.tailrec

object Utils {
  @tailrec
  def consumeText(lines: Seq[String], strAcc: String = "", seqAcc: Seq[String] = Seq.empty): Seq[String] = {
    lines.headOption match {
      case None => seqAcc :+ strAcc
      case Some("") => consumeText(lines.tail, "", seqAcc :+ strAcc)
      case Some(str) => consumeText(lines.tail, s"$strAcc$str ", seqAcc)
    }
  }
}
