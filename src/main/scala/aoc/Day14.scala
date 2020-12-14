package aoc

object Day14 extends App {
  val maskRegex = "mask = (\\w+)".r
  val memRegex = "mem\\[(\\d+)] = (\\d+)".r

  def binaryToLong(binary: String): Long = {
    binary
      .reverse
      .zipWithIndex
      .foldLeft(BigInt(0)) {
        case (acc, ('1', index)) => acc.setBit(index)
        case (acc, _)            => acc
      }
      .toLong
  }

  def solution1(memInstructions: Seq[String]): Long = {
    def applyMask(mask: String, value: String): String = {
      val binaryValue = value.toInt.toBinaryString

      (("0" * (36 - binaryValue.length)) + binaryValue)
        .zip(mask)
        .map { case (valueBit, maskBit) =>
          if(maskBit != 'X') maskBit
          else valueBit
        }
        .foldLeft("")(_ + _)
    }

    val (_, memStorage) = memInstructions
      .foldLeft(("", Map.empty[Int, Long])) {
        case ((_, storage), maskRegex(mask)) => mask -> storage
        case ((currMask, storage), memRegex(location, value)) =>
          val maskedBitString = applyMask(currMask, value)
          val resolvedLong = binaryToLong(maskedBitString)

          currMask -> storage.updated(location.toInt, resolvedLong)
        }

    memStorage.foldLeft(0L) { case (acc, (_, value)) =>
      acc + value
    }
  }

  def solution2(memInstructions: Seq[String]): Long = {
    def applyMask(mask: List[Char], binaryMemAddress: List[Char], addressAcc: String = "", listAcc: List[String] = Nil): Seq[String] = {
      mask.zip(binaryMemAddress) match {
        case ('0', memBit) :: Nil => listAcc :+ (addressAcc + memBit)
        case ('1', _) :: Nil => listAcc :+ (addressAcc + '1')
        case ('X', _) :: Nil =>
          (0 to 1).map { bit =>
            addressAcc + bit.toString.charAt(0)
          } ++ listAcc
        case ('0', memBit) :: tail =>
          val (maskTail, memTail) = tail.unzip
          applyMask(maskTail, memTail, addressAcc + memBit, listAcc)
        case ('1', _) :: tail =>
          val (maskTail, memTail) = tail.unzip
          applyMask(maskTail, memTail, addressAcc + '1', listAcc)
        case ('X', _) :: tail =>
          val (maskTail, memTail) = tail.unzip
          (0 to 1).foldLeft(listAcc) { case (acc, bit) =>
            acc ++ applyMask(maskTail, memTail, addressAcc + bit.toString.charAt(0))
          }
      }
    }

    val (_, memStorage) = memInstructions
      .foldLeft(("", Map.empty[Long, Long])) { case ((currMask, storage), instruction) =>
        instruction match {
          case maskRegex(mask) => mask -> storage
          case memRegex(location, value) =>
            val binaryMemAddress = location.toInt.toBinaryString
            val memoryAddresses = applyMask(currMask.toList, (("0" * (36 - binaryMemAddress.length)) + binaryMemAddress).toList)

            val updatedStorage = memoryAddresses.foldLeft(storage) { case (acc, memAddress) =>
              val resolvedLong = binaryToLong(memAddress)
              acc.updated(resolvedLong, value.toLong)
            }

            currMask -> updatedStorage
        }
      }

    memStorage.foldLeft(0L) { case (acc, (_, value)) =>
      acc + value
    }
  }

  val memoryInstructions = io.Source.fromResource("day14.txt").getLines.toSeq

  println(solution1(memoryInstructions))
  println(solution2(memoryInstructions))
}
