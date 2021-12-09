package aoc2021

object Utils {
  case class Point(x: Int, y: Int) {
    def inBounds(maxX: Int, maxY: Int): Boolean = {
      x >= 0 && y >= 0 && x < maxX && y < maxY
    }

    def findValidNeighbors(maxX: Int, maxY: Int)
                          (isValid: Point => Boolean = _ => true): List[Point] = {
      List(Point(x-1,y), Point(x+1,y), Point(x,y-1), Point(x,y+1))
        .filter { neighbor =>
          neighbor.inBounds(maxX, maxY) && isValid(neighbor)
        }
    }
  }
}
