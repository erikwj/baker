package com.ing.baker.compiler

import io.kagera.api._
import io.kagera.api.colored._
import io.kagera.api.multiset._

object RecipeAnalysis {

  def isDeliverable(recipe: CompiledRecipe, ingredients: Set[String]): Boolean = {

    val places: Set[Place[_]] = ingredients.flatMap(name => recipe.petriNet.places.find(_.label == name))
    val target: MultiSet[Place[_]] = places.map(p => p -> 1).toMap

    val sensoryTransitions = recipe.sensoryEvents.flatMap(c => recipe.petriNet.transitions.find(_.label == c.getSimpleName))

    val helper = new PetriNetHelper(recipe.petriNet)

    val initialMarking = sensoryTransitions.foldLeft(recipe.initialMarking.multiplicities) {
      case (m, t) => helper.fire(m, t)
    }

    helper.isReachable(initialMarking, target, sensoryTransitions)
  }

  class PetriNetHelper[P, T](pn: PetriNet[P, T]) {

    val inMarking = pn.transitions.map(t => t -> pn.inMarking(t)).toMap
    val outMarking = pn.transitions.map(t => t -> pn.outMarking(t)).toMap

    val coldTransitions = pn.transitions.filter(t ⇒ pn.incomingPlaces(t).isEmpty)

    def enabledTransitions(m0: MultiSet[P]) = {

      val outAdjancent = m0.keys.map(pn.outgoingTransitions).reduceOption(_ ++ _).getOrElse(Set.empty).
        filter(t ⇒ m0.isSubSet(inMarking(t)))

      coldTransitions ++ outAdjancent
    }

    def intersects(m1: MultiSet[P], m2: MultiSet[P]) = m1.exists {
      case (p, n) => m2.getOrElse(p, 0) > 0
    }

    def fire(m0: MultiSet[P], t: T): MultiSet[P] =
      m0.multisetDifference(inMarking(t))
        .multisetSum(outMarking(t))

    def fireAll(m0: MultiSet[P], transitions: Iterable[T]): MultiSet[P] = {
      transitions.foldLeft(m0) {
        case (m, t) => fire(m, t)
      }
    }

    def nonConflicting(transitions: Set[T]) = {
      transitions.filter { t =>
        val sumMarking = (transitions - t).foldLeft(MultiSet.empty[P]) {
          case (m, tn) => m.multisetSum(inMarking(t))
        }

        intersects(sumMarking, inMarking(t))
      }
    }

    def isReachable(marking: MultiSet[P], target: MultiSet[P], filter: Set[T] = Set.empty): Boolean = {
        // fire all
        val enabled = enabledTransitions(marking) -- filter

        val check = marking
//          if (!intersects(target, m0)) {
//          fireAll(m0, nonConflicting(enabled))
//        } else
//         m0

        if (marking.isSubSet(target))
          true
        else
          enabled.view.map(t => isReachable(fire(marking, t), target, filter)).exists(_ == true)
    }

    def check(m0: MultiSet[P], target: MultiSet[P], enabled: Set[T]) = {

      if (!intersects(target, m0)) {
        fireAll(m0, nonConflicting(enabled))
      } else
        m0
    }
  }
}
