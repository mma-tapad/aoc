package aoc2021

object Day12 extends App {
  def populateNodeMap(entry: String, dest: String, nodeMap: Map[String, Set[String]]): Map[String, Set[String]] = {
    nodeMap
      .get(entry)
      .fold {
        nodeMap.updated(entry, Set(dest))
      } { paths =>
        nodeMap.updated(entry, paths + dest)
      }
      .view
      .mapValues(_.excl("start"))
      .toMap
  }

  def solution1(lines: List[String]): Int = {
    def countPaths(paths: Set[String], counter: Int = 0, visited: Set[String] = Set("start"))
                  (nodeMap: Map[String, Set[String]] = Map.empty): Int = {
      paths.foldLeft(counter) { case (currCounter, path) =>
        path match {
          case "end" => currCounter+1
          case node if node.toLowerCase == node && visited.contains(node) => currCounter
          case node  => countPaths(nodeMap(node), currCounter, visited+node)(nodeMap)
        }
      }
    }

    val nodeMap: Map[String, Set[String]] = lines.foldLeft(Map.empty[String, Set[String]]) { case (accMap, line) =>
      val split = line.split("-")
      populateNodeMap(split.last, split.head, populateNodeMap(split.head, split.last, accMap))
    }

    countPaths(nodeMap("start"))(nodeMap)
  }

  def solution2(lines: List[String]): Int = {
    def countPaths2(paths: Set[String],
                    counter: Int = 0,
                    visitCount: Map[String, Int] = Map.empty,
                    visitLimit: Boolean = false
                   )(nodeMap: Map[String, Set[String]] = Map.empty): Int = {
      paths.foldLeft(counter) { case (currCounter, path) =>
        (path, visitCount.get(path), path.toLowerCase == path, visitLimit) match {
          case ("end", _, _, _)   => currCounter+1
          case (node, None, true, _) => countPaths2(nodeMap(node), currCounter, visitCount.updated(node, 1), visitLimit)(nodeMap)
          case (node, None, false, _) => countPaths2(nodeMap(node), currCounter, visitCount, visitLimit)(nodeMap)
          case (_, Some(1), true, true) => currCounter
          case (_, Some(2), true, true) => currCounter
          case (node, Some(1), true, false) => countPaths2(nodeMap(node), currCounter, visitCount.updated(node, 2), visitLimit = true)(nodeMap)
          case (node, _, _, _)  => countPaths2(nodeMap(node), currCounter, visitCount, visitLimit)(nodeMap)
        }
      }
    }

    val nodeMap: Map[String, Set[String]] = lines.foldLeft(Map.empty[String, Set[String]]) { case (accMap, line) =>
      val split = line.split("-")
      populateNodeMap(split.last, split.head, populateNodeMap(split.head, split.last, accMap))
    }

    countPaths2(nodeMap("start"))(nodeMap)
  }

  val lines = io.Source.fromResource("2021/day12.txt").getLines.toList

  println(solution1(lines))
  println(solution2(lines))
}
