package com.ing.baker.compiler

import com.ing.baker.TestRecipeHelper
import com.ing.baker.api.Event
import com.ing.baker.core.Interaction
import com.ing.baker.java_api.{ProcessId, ProvidesIngredient, RequiresIngredient}
import com.ing.baker.scala_api.{InteractionDescriptorFactory, SRecipe}

class RecipeAnalysisSpec extends TestRecipeHelper {

  trait BakePizza extends Interaction {
    @ProvidesIngredient("pizza")
    def apply(@RequiresIngredient("coldPizza") pizza: String): String
  }

  trait MixIngredients extends Interaction {
    @ProvidesIngredient("dough")
    def apply(@RequiresIngredient("bowl") bowl: String, @RequiresIngredient("fork") fork: String, @RequiresIngredient("yeast") yeast: String,
              @RequiresIngredient("sugar") sugar: String, @RequiresIngredient("oliveOil") oliveOil: String, @RequiresIngredient("water") water: String,
              @RequiresIngredient("sievedFlour") sievedFlour: Boolean): String
  }

  trait SieveFlour extends Interaction {
    @ProvidesIngredient("sievedFlour")
    def apply(@RequiresIngredient("sieve") sieve: String, @RequiresIngredient("flour") flour: String, @RequiresIngredient("salt") salt: String): String
  }

  trait KneedDough extends Interaction {
    @ProvidesIngredient("smoothSpringyDough")
    def apply(@ProcessId requestId: String, @RequiresIngredient("dough") dough: String): String
  }

  trait AddToppings extends Interaction {
    @ProvidesIngredient("coldPizza")
    def apply(@ProcessId requestId: String, @RequiresIngredient("pizzaBottom") pizzaBottom: String, @RequiresIngredient("tomatoSause") tomatoSause: String, @RequiresIngredient("cheese") cheese: String)
  }

  trait HeatPizza extends Interaction {
    @ProvidesIngredient("pizza")
    def apply(@RequiresIngredient("coldPizza") coldPizza: String, @RequiresIngredient("oven") oven: String): String
  }

  trait RollDough extends Interaction {
    @ProvidesIngredient("pizzaBottom")
    def apply(@RequiresIngredient("doughRoller") doughRoller: String, @RequiresIngredient("smoothSpringyDough") SmoothSpringyDough: String): String
  }

  trait HeatOven extends Interaction {
    //Event version
    def apply(@RequiresIngredient("oven") oven: String)
  }

  case class KitchenToolsAvailable(sieve: String, bowl: String, fork: String, doughRoller: String, oven: String) extends Event

  case class GroceriesDone(
    flour: String, salt: String, sugar: String, yeast: String, oliveOil: String, water: String,
    tomatoSause: String, cheese: String, ham: String, eggs: String, soda: String) extends Event

  case class OvenHeatedEvent() extends Event

  "The RecipeAnalysis" should {

    "be able to proof an end state can be reached" in {

      val recipe = SRecipe(name = "PizzaRecipe",
        interactions = Seq(
          InteractionDescriptorFactory[MixIngredients],
          InteractionDescriptorFactory[KneedDough],
          InteractionDescriptorFactory[RollDough],
          InteractionDescriptorFactory[AddToppings],
          InteractionDescriptorFactory[HeatOven],
          InteractionDescriptorFactory[HeatPizza].withRequiredEvent[OvenHeatedEvent]),

        events = Set(classOf[KitchenToolsAvailable], classOf[GroceriesDone], classOf[OvenHeatedEvent]))

      val compiledRecipe: CompiledRecipe = recipe.compileRecipe

//      println(compiledRecipe.getRecipeVisualization)

      println(RecipeAnalysis.isDeliverable(compiledRecipe, Set("pizza")))
    }
  }
}
