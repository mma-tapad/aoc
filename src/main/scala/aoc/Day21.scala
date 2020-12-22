package aoc

object Day21 extends App {
  val ingredientRegex = """((\w+\s)+)\(""".r
  val allergenRegex = """\(contains ((\w+(, )?)+)\)""".r

  def solveAllergens(menu: Seq[String]): (Map[String, Set[String]], Map[String, Int]) = {
    menu
      .foldLeft(Map.empty[String, Set[String]], Map.empty[String, Int].withDefaultValue(0)) {
        case ((mapAcc, ingredientCount), line) =>
          val ingredients = ingredientRegex
            .findFirstMatchIn(line)
            .get
            .group(1)
            .trim
            .split(" ")

          val allergens = allergenRegex
            .findFirstMatchIn(line)
            .get
            .group(1)
            .split(", ")
            .toSet

          val ingredientSet = ingredients.toSet

          allergens
            .foldLeft(mapAcc) { case (acc, allergen) =>
              acc.get(allergen) match {
                case Some(ingreds) => acc.updated(allergen, ingreds intersect ingredientSet)
                case _ => acc.updated(allergen, ingredientSet)
              }
            } -> ingredients.foldLeft(ingredientCount) { case (iAcc, ingredient) =>
              iAcc.updated(ingredient, iAcc(ingredient) + 1)
            }
      }
  }

  def solution1(menu: Seq[String]) = {
    val (allergenToIngredients, allIngredients) = solveAllergens(menu)

    val (allergicIngredients, _) = (0 until allergenToIngredients.size)
      .foldLeft((Set.empty[String], allergenToIngredients)) { case ((ingredientAcc, allergenMap), _) =>
        val (singleAllergenToIngredient, updatedAllergenMap) = allergenMap.partition(_._2.size == 1)
        (ingredientAcc ++ singleAllergenToIngredient.values.flatten) ->
          singleAllergenToIngredient.values.foldLeft(updatedAllergenMap) { case (acc, ai) =>
            acc.map { case (k,v) =>
              k -> (v -- ai)
            }
          }
      }

    (allIngredients -- allergicIngredients).values.sum
  }

  def solution2(menu: Seq[String]) = {
    val allergenToIngredients = solveAllergens(menu)._1

    (0 until allergenToIngredients.size)
      .foldLeft((Map.empty[String, String], allergenToIngredients)) { case ((ingredientAcc, allergenMap), _) =>
        val (singleAllergenToIngredient, updatedAllergenMap) = allergenMap.partition(_._2.size == 1)
        singleAllergenToIngredient.foldLeft(ingredientAcc) { case (iAcc, (singleAllergen, singleIngredient)) =>
          iAcc.updated(singleAllergen, singleIngredient.head)
        } ->
          singleAllergenToIngredient.values.foldLeft(updatedAllergenMap) { case (acc, ai) =>
            acc.map { case (k,v) =>
              k -> (v -- ai)
            }
          }
      }
      ._1
      .toSeq
      .sortBy(_._1)
      .map(_._2)
      .mkString(",")
  }


  val menu = io.Source.fromResource("day21.txt").getLines.toSeq

  println(solution1(menu))
  println(solution2(menu))
}
