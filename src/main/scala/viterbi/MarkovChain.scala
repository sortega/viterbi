package viterbi

case class MarkovChain[S](prob: Map[S, Distro[S]]) {

  def states: Set[S] = prob.keySet
}

object MarkovChain {

}
