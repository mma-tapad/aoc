//package aoc2021
//
//import java.math.BigInteger
//import scala.annotation.tailrec
//
//object Day16 extends App {
//  val typeIdToOperator = Map(
//    0 -> SUM,
//    1 -> PRODUCT,
//    2 -> MIN,
//    3 -> MAX,
//    5 -> GT,
//    6 -> LT,
//    7 -> EQ,
//  )
//
//  @tailrec
//  def readPacket(curr: String, rest: String, mode: ReadingMode)
//                (versionSum: Long,
//                 literalBuilder: String,
//                 literalStore: List[Either[Operator, Long]]): (Long, List[Either[Operator, Long]]) = {
//
//    (curr, mode) match {
//      case ("", _) => versionSum -> literalStore
//      case (_, VERSION) =>
//        val version = new BigInteger(curr,2).longValue()
//        readPacket(rest.take(3), rest.drop(3), TYPEID)(versionSum + version, "", literalStore)
//
//      case (_, TYPEID) =>
//        Integer.parseInt(curr,2) match {
//          case 4 => readPacket(rest.take(5), rest.drop(5), LITERAL)(versionSum, "", literalStore)
//          case typeId => readPacket(rest.take(1), rest.drop(1), LENGTHTYPEID)(versionSum, "", Left(typeIdToOperator(typeId)) :: literalStore)
//        }
//
//      case (_, LITERAL) =>
//        curr.head match {
//          case '1' => readPacket(rest.take(5), rest.drop(5), LITERAL)(versionSum, literalBuilder + curr.drop(1), literalStore)
//          case '0' =>
//            val literal = new BigInteger(literalBuilder + curr.drop(1),2).longValue()
//            readPacket(rest.take(3), rest.drop(3), VERSION)(versionSum, "", Right(literal) :: literalStore)
//        }
//
//      case (_, LENGTHTYPEID) =>
//        curr match {
//          case "0" => readPacket(rest.take(15), rest.drop(15), BITLENGTH)(versionSum, "", literalStore)
//          case "1" => readPacket(rest.take(11), rest.drop(11), NUMSUBPACKETS)(versionSum, "", literalStore)
//        }
//
//      case (_, BITLENGTH) =>
//        val length = new BigInteger(curr,2).intValue()
//        //TODO do we care yet?
////        readPacket(rest.take(3), rest.drop(3), VERSION)(versionSum, "", literalStore, packetQueue)
//        val subpacket = readPacket(rest.take(3), rest.slice(3,length), VERSION)(versionSum, "", literalStore)//rest.drop(length) :: packetQueue)
//
//
//      case (_, NUMSUBPACKETS) =>
//        val numpackets = new BigInteger(curr,2).longValue()
//        //TODO do we care yet?
////        readPacket(rest.take(3), rest.drop(3), VERSION)(versionSum, "", literalStore, packetQueue)
//        readPacket(rest.take(3), rest.slice(3,length), VERSION)(versionSum, "", literalStore)
//    }
//  }
//
//  def parseOperators(literals: List[Either[Operator, Long]]): List[Long] = {
//    literals match {
//      case Left(op) :: tail =>
//        val values = parseOperators(tail)
//
//      case Right(num) :: tail =>
//    }
//  }
//
//  def solution1(packet: String) = {
//    readPacket(packet.take(3), packet.drop(3), VERSION)(0L, "", Nil)._1
//  }
//
//  def solution2(packet: String) = {
//    readPacket(packet.take(3), packet.drop(3), VERSION)(0L, "", Nil)
//  }
//
//  val packet = io.Source.fromResource("2021/day16.txt")
//    .getLines
//    .toList
//    .flatten
//    .map { c =>
//      val binary = Integer.parseInt(c.toString, 16).toBinaryString
//      "0000".take(4-binary.length) + binary
//    }
//    .mkString("")
//
//  println(solution1(packet))
////  println(solution2(lines))
//}
//
//sealed trait ReadingMode
//case object VERSION extends ReadingMode
//case object TYPEID extends ReadingMode
//case object LITERAL extends ReadingMode
//case object LENGTHTYPEID extends ReadingMode
//case object BITLENGTH extends ReadingMode
//case object NUMSUBPACKETS extends ReadingMode
//case object SUBPACKET extends ReadingMode
//
//sealed trait Operator
//case object SUM extends Operator
//case object PRODUCT extends Operator
//case object MIN extends Operator
//case object MAX extends Operator
//case object GT extends Operator
//case object LT extends Operator
//case object EQ extends Operator