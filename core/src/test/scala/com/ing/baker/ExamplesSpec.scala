package com.ing.baker

import java.util.UUID

class ExamplesSpec extends TestRecipeHelper {
  "An example web-shop recipe" should {
    "be represented in a DOT format that is suitable for visualization" in {
      println(getWebshopRecipe.compileRecipe.getRecipeVisualization)
    }

    "have no validation errors" in {
      getWebshopRecipe().compileRecipe.validationErrors.filterNot((s) => s.startsWith("No implementation provided")) shouldBe empty
    }

    "be executable" in {
      val baker = setupBakerWithWebshopRecipe()

      val processId = UUID.randomUUID()
      //handling each order will result in an new process instance from the recipe
      baker.bake(processId)

      baker.shutdown()
    }
  }
}
