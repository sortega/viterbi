package viterbi

case class HiddenMarkovChain[S, O](prevalence: Distro[S],
                                   hiddenChain: MarkovChain[S],
                                   observationDistros: Map[S, Distro[O]]) {

  lazy val observations: Set[O] = observationDistros.values.flatMap(_.prob.keys).toSet

  def transitionProbGivenObservation(prevState: Option[S], obs: O): Distro[S] =
    transitionProb(prevState)
      .flatMap(s => observationDistros(s).map(o => (s, o)))
      .filter {
        case (_, `obs`) => true
        case _ => false
      }
      .map(_._1) // TODO: filter+map => Distro.collect

  def transitionProb(prevState: Option[S]): Distro[S] = prevState match {
    case None => prevalence
    case Some(state) => hiddenChain.prob(state)
  }

  def viterbiPath(observations: List[O]): List[S] = {
    // TODO: memoize to get polynomial time
    def viterbiPaths(observations: List[O]): Map[List[S], Double] = observations match {
      case Nil => Map(Nil -> 1d)
      case currentObs :: history =>
        val bestPaths = viterbiPaths(history)
        hiddenChain.states.map { state =>
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
