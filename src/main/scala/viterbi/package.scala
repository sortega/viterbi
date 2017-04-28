package object viterbi {
  val Epsilon = 0.0001

  def maxByWeight[A](map: Map[A, Double]): A = map.maxBy(_._2)._1
}
