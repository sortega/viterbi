package viterbi

import scalaz.Memo

case class HiddenMarkovChain[S, O](prevalence: Distro[S],
                                   hiddenChain: Map[S, Distro[S]],
                                   observationDistros: Map[S, Distro[O]]) {

  def states: Set[S] = hiddenChain.keySet

  lazy val observations: Set[O] = observationDistros.values.flatMap(_.prob.keys).toSet

  def transitionProbGivenObservation(prevState: Option[S], obs: O): Distro[S] =
    transitionProb(prevState)
      .flatMap(s => observationDistros(s).map(o => (s, o)))
      .collect {
        case (event, `obs`) => event
      }

  def transitionProb(prevState: Option[S]): Distro[S] = prevState match {
    case None => prevalence
    case Some(state) => hiddenChain(state)
  }

  def viterbiPath(observations: List[O]): List[S] = {
    lazy val viterbiPaths: List[O] => Map[List[S], Double] = Memo.mutableHashMapMemo {
      case Nil => Map(Nil -> 1d)
      case currentObs :: history =>
        val bestPaths = viterbiPaths(history)
        states.map { state =>
          bestPaths
            .map {
              case (path, prob) =>
                (state :: path,
                 prob * transitionProbGivenObservation(path.headOption, currentObs).apply(state))
            }
            .maxBy(_._2)
        }.toMap
    }

    maxByWeight(viterbiPaths(observations.reverse)).reverse
  }
}
