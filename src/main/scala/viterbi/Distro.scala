package viterbi

/** Discrete probability distribution
  *
  * @tparam E    Type of the discrete events
  * @param prob  Probability per event
  */
case class Distro[E](prob: Map[E, Double]) {
  require((1 - prob.values.sum.abs) < Epsilon, s"Probs doesn't add up to one: $this")

  def apply(event: E): Double = prob.getOrElse(event, 0d)

  def map[E2](f: E => E2): Distro[E2] =
    Distro(prob.keys.groupBy(f).map {
      case (e2, events) =>
        e2 -> events.map(prob).sum
    })

  def flatMap[E2](f: E => Distro[E2]): Distro[E2] = {
    val combinations: List[(E2, Double)] = for {
      (e, p1) <- prob.toList
      (e2, p2) <- f(e).prob
    } yield (e2, p1 * p2)
    Distro(combinations.groupBy(_._1).mapValues(_.map(_._2).sum))
  }

  def filter(pred: E => Boolean): Distro[E] = {
    val survivingEvents = prob.filterKeys(pred)
    require(survivingEvents.nonEmpty, s"filter $pred discards all options")
    Distro.weighted(survivingEvents)
  }

  def collect[E2](select: PartialFunction[E, E2]): Distro[E2] =
    filter(select.isDefinedAt).map(select.apply)
}

object Distro {
  def apply[E](pairs: (E, Double)*): Distro[E] = {
    val map = pairs.toMap
    require(map.size == pairs.size, s"Repeated events: $pairs")
    Distro(map)
  }

  def unit[E](value: E): Distro[E] = Distro(Map(value -> 1d))

  def uniform[E](values: Seq[E]): Distro[E] = {
    val p = 1d / values.size
    Distro(values.map(v => v -> p).toMap)
  }

  def weighted[E](weightedValues: Map[E, Double]): Distro[E] = {
    require(weightedValues.values.forall(_ >= 0), s"Negative weights: $weightedValues")
    val totalWeight = weightedValues.values.sum
    require(totalWeight > 0, s"Zero total weight: $totalWeight")
    Distro(weightedValues.mapValues(_ / totalWeight))
  }
}
