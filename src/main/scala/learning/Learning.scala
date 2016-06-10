package drl.learning

import drl.mdp.MDP._
import drl.backend.Backend._

abstract class RL[S: Statable, B: NeuralN]() extends Iterator[PreBatch[S]] {

}
