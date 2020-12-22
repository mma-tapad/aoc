package aoc

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.{Success, Try}

object Day17 extends App {

  def solution1(space: Array[Array[Array[Char]]], iterations: Int = 6) = {
    val directions: Seq[(Int => Int, Int => Int, Int => Int)] = {
      val functions = Seq[Int => Int](identity, _ + 1, _ - 1)
      //cross product for every adjacent direction
      (for {
        x <- functions
        y <- functions
        z <- functions
      } yield
        (z, y, x)
        ).tail // remove the (identity, identity, identity) 3ple
    }

    def deepCopy[T: ClassTag](arr: Array[Array[Array[T]]]): Array[Array[Array[T]]] = {
      arr.map(_.map(_.clone()))
    }

    def countActiveNeighbors(currSpace: Array[Array[Array[Char]]])(x: Int, y: Int, z: Int): Int = {
      directions.count { case (zFn, yFn, xFn) =>
        Try(currSpace(zFn(z))(yFn(y))(xFn(x))) == Success('#')
      }
    }

    @tailrec
    def iterate(
      startSpace: Array[Array[Array[Char]]],
      destSpace: Array[Array[Array[Char]]],
      iterations: Int
    ): Array[Array[Array[Char]]] = {
      iterations match {
        case 0 => destSpace
        case _ =>
          for {
            z <- startSpace.indices
            y <- startSpace(z).indices
            x <- startSpace(z)(y).indices
          } {
            val activeNeighbors = countActiveNeighbors(startSpace)(x, y, z)
            if(activeNeighbors == 3) {
              destSpace(z)(y)(x) = '#'
            } else if(activeNeighbors == 2) {
              destSpace(z)(y)(x) = startSpace(z)(y)(x)
            } else {
              destSpace(z)(y)(x) = '.'
            }
          }

          iterate(destSpace, deepCopy(destSpace), iterations - 1)
      }
    }

    val expandedSpace: Array[Array[Array[Char]]] = {
      val doubleSize = iterations * 2

      val expanded = Array.fill(space.length + doubleSize)(
        Array.fill(space.head.length + doubleSize)(
          Array.fill(space.head.head.length + doubleSize)('.')
        )
      )

      for {
        z <- space.indices
        y <- space(z).indices
        x <- space(z)(y).indices
      } {
        expanded(z + iterations)(y + iterations)(x + iterations) = space(z)(y)(x)
      }

      expanded
    }

    iterate(expandedSpace, deepCopy(expandedSpace), 6)
      .foldLeft(0) { case (zacc, yxGrid) =>
        zacc + yxGrid.foldLeft(0) { case (yacc, xGrid) =>
          yacc + xGrid.count(_ == '#')
        }
      }
  }

  def solution2(space: Array[Array[Array[Char]]], iterations: Int = 6) = {
    val directions: Seq[(Int => Int, Int => Int, Int => Int, Int => Int)] = {
      val functions = Seq[Int => Int](identity, _ + 1, _ - 1)
      //cross product for every adjacent direction
      (for {
        x <- functions
        y <- functions
        z <- functions
        w <- functions
      } yield
        (w, z, y, x)
        ).tail // remove the (identity, identity, identity, identity) 4ple
    }

    def deepCopy[T: ClassTag](arr: Array[Array[Array[Array[T]]]]): Array[Array[Array[Array[T]]]] = {
      arr.map(_.map(_.map(_.clone())))
    }

    def countActiveNeighbors(currSpace: Array[Array[Array[Array[Char]]]])(x: Int, y: Int, z: Int, w: Int): Int = {
      directions.count { case (wFn, zFn, yFn, xFn) =>
        Try(currSpace(wFn(w))(zFn(z))(yFn(y))(xFn(x))) == Success('#')
      }
    }

    @tailrec
    def iterate(
      startSpace: Array[Array[Array[Array[Char]]]],
      destSpace: Array[Array[Array[Array[Char]]]],
      iterations: Int
    ): Array[Array[Array[Array[Char]]]] = {
      iterations match {
        case 0 => destSpace
        case _ =>
          for {
            w <- startSpace.indices
            z <- startSpace(w).indices
            y <- startSpace(w)(z).indices
            x <- startSpace(w)(z)(y).indices
          } {
            val activeNeighbors = countActiveNeighbors(startSpace)(x, y, z, w)
            if(activeNeighbors == 3) {
              destSpace(w)(z)(y)(x) = '#'
            } else if(activeNeighbors == 2) {
              destSpace(w)(z)(y)(x) = startSpace(w)(z)(y)(x)
            } else {
              destSpace(w)(z)(y)(x) = '.'
            }
          }

          iterate(destSpace, deepCopy(destSpace), iterations - 1)
      }
    }

    val wSpace = Array(space)

    val expandedSpace: Array[Array[Array[Array[Char]]]] = {
      val doubleSize = iterations * 2

      val expanded = Array.fill(wSpace.length + doubleSize)(
          Array.fill(wSpace.head.length + doubleSize)(
            Array.fill(wSpace.head.head.length + doubleSize)(
              Array.fill(wSpace.head.head.head.length + doubleSize)('.')
            )
          )
      )

      for {
        w <- wSpace.indices
        z <- wSpace(w).indices
        y <- wSpace(w)(z).indices
        x <- wSpace(w)(z)(y).indices
      } {
        expanded(w + iterations)(z + iterations)(y + iterations)(x + iterations) = wSpace(w)(z)(y)(x)
      }

      expanded
    }

    iterate(expandedSpace, deepCopy(expandedSpace), 6)
      .foldLeft(0) { case (wacc, zyxGrid) =>
        wacc + zyxGrid.foldLeft(0) { case (zacc, yxGrid) =>
          zacc + yxGrid.foldLeft(0) { case (yacc, xGrid) =>
            yacc + xGrid.count(_ == '#')
          }
        }
      }
  }

  val space: Array[Array[Array[Char]]] =
    Array(
      io.Source.fromResource("day17.txt").getLines.toArray
        .map(_.toCharArray)
    )

  println(solution1(space))
  println(solution2(space))
}
