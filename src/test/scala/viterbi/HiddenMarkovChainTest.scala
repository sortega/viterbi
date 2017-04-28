package viterbi

import org.scalatest.{FlatSpec, Matchers}

class HiddenMarkovChainTest extends FlatSpec with Matchers {

  "The wikipedia example" should "work" in {
    sealed trait Symptom
    object Symptom {
      case object Normal extends Symptom
      case object Cold extends Symptom
      case object Dizzy extends Symptom
    }

    sealed trait Health
    object Health {
      case object Good extends Health
      case object Feverish extends Health
    }

    val hidden = HiddenMarkovChain[Health, Symptom](
      prevalence = Distro(Health.Good -> 0.6, Health.Feverish -> 0.4),
      hiddenChain = Map(
        Health.Good -> Distro(Health.Good -> 0.7d, Health.Feverish -> 0.3d),
        Health.Feverish -> Distro(Health.Good -> 0.4d, Health.Feverish -> 0.6d)
      ),
      observationDistros = Map(
        Health.Good -> Distro(Symptom.Normal -> 0.5, Symptom.Cold -> 0.4, Symptom.Dizzy -> 0.1),
        Health.Feverish -> Distro(Symptom.Normal -> 0.1, Symptom.Cold -> 0.3, Symptom.Dizzy -> 0.6)
      )
    )

    hidden.viterbiPath(List(Symptom.Normal, Symptom.Cold, Symptom.Dizzy)) shouldBe List(
      Health.Good,
      Health.Good,
      Health.Feverish
    )
  }

  "An example HMM with more skewed distros" should "be able of reinterpret last hidden state" in {
    sealed trait Symptom
    object Symptom {
      case object Normal extends Symptom
      case object LowFever extends Symptom
      case object HighFever extends Symptom
    }

    sealed trait Health
    object Health {
      case object Good extends Health
      case object Incubating extends Health
      case object Ill extends Health
    }

    val hidden = HiddenMarkovChain[Health, Symptom](
      prevalence = Distro(Health.Good -> 0.5, Health.Incubating -> 0.3, Health.Ill -> 0.2),
      hiddenChain = Map(
        Health.Good -> Distro(Health.Good -> 0.9d, Health.Incubating -> 0.1d),
        Health.Incubating -> Distro(Health.Good -> 0.1d,
                                    Health.Incubating -> 0.6d,
                                    Health.Ill -> 0.3d),
        Health.Ill -> Distro(Health.Good -> 0.2d, Health.Ill -> 0.8d)
      ),
      observationDistros = Map(
        Health.Good -> Distro(Symptom.Normal -> 0.95,
                              Symptom.LowFever -> 0.04,
                              Symptom.HighFever -> 0.01),
        Health.Incubating -> Distro(Symptom.Normal -> 0.6,
                                    Symptom.LowFever -> 0.35,
                                    Symptom.HighFever -> 0.05),
        Health.Ill -> Distro(Symptom.Normal -> 0.05,
                             Symptom.LowFever -> 0.2,
                             Symptom.HighFever -> 0.75)
      )
    )

    hidden.viterbiPath(List(Symptom.Normal, Symptom.LowFever)) shouldBe List(
      Health.Good,
      Health.Good
    )
    hidden.viterbiPath(List(Symptom.Normal, Symptom.LowFever, Symptom.HighFever)) shouldBe List(
      Health.Good,
      Health.Incubating,
      Health.Ill
    )
  }

}
