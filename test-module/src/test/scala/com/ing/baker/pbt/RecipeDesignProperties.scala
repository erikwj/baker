package com.ing.baker.pbt

import java.io.{File, PrintWriter}

import com.ing.baker.compiler.RecipeCompiler
import com.ing.baker.il.CompiledRecipe
import com.ing.baker.recipe.common
import com.ing.baker.recipe.common.FiresOneOfEvents
import com.ing.baker.recipe.scaladsl.{Event, Ingredient, Interaction, InteractionDescriptor, Recipe}
import org.scalacheck._

import scala.util.Random

class RecipeDesignProperties extends Properties("Design-time properties") {
  import Prop.forAll
  import RecipeDesignProperties._

  //generate events


  //weave them into a recipe
  property("compiles with no errors") = forAll(recipesGen) { recipe =>
    val compiledRecipe = RecipeCompiler.compileRecipe(recipe)

    println(s"PBT stats: recipeName: ${compiledRecipe.name} " +
      s"nrOfAllIngredients: ${compiledRecipe.ingredients.size} " +
      s"nrOfSensoryEvents: ${compiledRecipe.sensoryEvents.size} " +
      s"nrOfInteractionEvents: ${compiledRecipe.interactionEvents.size} " +
      s"nrOfInteractions: ${compiledRecipe.interactionTransitions.size}")

    if (compiledRecipe.validationErrors.nonEmpty) {
      println(s"Validation errors: ${compiledRecipe.validationErrors}")
      println(s"Visual recipe: ${compiledRecipe.getRecipeVisualization}")
    }

    dumpVisualRecipe(recipeVisualizationOutputPath, compiledRecipe)

    compiledRecipe.validationErrors.isEmpty
  }
}

object RecipeDesignProperties {

  val maxNrOfIngredientsPerEvent = 1
  val recipeVisualizationOutputPath: String = System.getProperty("java.io.tmpdir")

  val eventGen = for {
    name <- Gen.identifier
    providedIngredients <- Gen.listOfN(maxNrOfIngredientsPerEvent, ingredientGen)
  } yield Event(name = name, providedIngredients)

  val ingredientGen = for {
    name <- Gen.identifier
  } yield Ingredient(name)

  val recipesGen = for {
    name <- Gen.identifier
    sensoryEvents <- Gen.listOfN(1, eventGen)
    interactions <- interactionsGen(getIngredientsFrom(sensoryEvents))
  } yield Recipe(name)
    //turn the lists into var args
    .withSensoryEvents(sensoryEvents: _*)
    .withInteractions(interactions: _*)

  def interactionsGen(ingredients: Seq[common.Ingredient]): Gen[List[InteractionDescriptor]] = {
    Gen.const(getInteractions(ingredients))
  }

  def getInteractions(withIngredients: Seq[common.Ingredient]): List[InteractionDescriptor] = {
    def rec(ingredients: List[common.Ingredient], acc: List[InteractionDescriptor]): List[InteractionDescriptor] = ingredients match {
      case Nil => {
        println("empty, returning " + acc.length)
        acc
      }
      case ingredientsLeft => {
        val requiredIngredients = Random.shuffle(ingredientsLeft).take(Gen.choose(0, ingredientsLeft.length).sample.getOrElse(0))

        requiredIngredients match {

          case Nil => {
            rec(ingredientsLeft, acc)
          }

          case _ => {
            val output = getDescriptor(requiredIngredients)
            if(ingredients.diff(requiredIngredients).length == 0)
              output._1 :: acc
            else
              rec(ingredients.diff(requiredIngredients) ++ output._2, output._1 :: acc)
          }
        }
      }
    }

    rec(withIngredients.toList, List.empty[InteractionDescriptor])
  }

  def getDescriptor(ingredients: Seq[common.Ingredient]): (InteractionDescriptor, List[common.Ingredient]) = {
    val output = new FiresOneOfEvents(Seq(eventGen.sample.get))
    val interaction = new Interaction(Gen.alphaNumStr.sample.get, ingredients, output)
    (InteractionDescriptor.apply(interaction), output.events.flatMap(e => e.providedIngredients).toList)
  }

  def getIngredientsFrom(events: List[Event]): Seq[common.Ingredient] =
    events.flatMap(event => event.providedIngredients)

  def dumpVisualRecipe(dumpDir: String, compiledRecipe: CompiledRecipe): Unit = {
    val fileName =
      if (dumpDir endsWith "/") s"$dumpDir${compiledRecipe.name}.dot"
      else s"$dumpDir/${compiledRecipe.name}.dot"

    val writer = new PrintWriter(new File(fileName))

    try {
      println(s"Dumping the visual recipe here: $fileName")
      writer.write(compiledRecipe.getRecipeVisualization)
    } finally {
      writer.close()
    }
  }
}
