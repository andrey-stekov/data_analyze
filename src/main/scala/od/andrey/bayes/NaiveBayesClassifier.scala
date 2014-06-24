package od.andrey.bayes

import scala.math._
import scala.Tuple2

import od.andrey.common.Classifier

/**
 * Created by ALemeshev on 19.06.2014.
 */
class NaiveBayesClassifier [C, S] (val classes : Map[C, Double],
                                   val freq : Map[Tuple2[C, S], Double]) extends Classifier[C, S] {
  val MIN_PLACEHOLDER : Double = pow(10, -7)
  val LOG_PLACEHOLDER : Double = -log(MIN_PLACEHOLDER)

  def classify(features : List[S]): C = {
    classes.minBy((entry : Tuple2[C, Double]) => -log(entry._2) +
      features.foldLeft(0.0) {
        (acc : Double, feature : S) =>
          acc + -log(if (freq.contains((entry._1, feature))) {
            freq((entry._1, feature))
          } else {
            MIN_PLACEHOLDER
          })
      }
    )._1
  }
}

object NaiveBayesClassifier {
  def buildClassifier[C, S](samples : List[Tuple2 [C, S]]) : NaiveBayesClassifier[C, S] = {
    val classes : Map[C, Double] = samples.foldLeft(Map[C, Double]()) {
      (acc : Map[C, Double], sample : Tuple2 [C, S]) =>
        acc + ((sample._1, if (acc.contains(sample._1)) { acc(sample._1) + 1 } else { 1 }))
    }

    val frequences : Map [Tuple2 [C, S], Double] = samples.foldLeft(Map [Tuple2 [C, S], Double]()) {
      (acc : Map [Tuple2 [C, S], Double], sample : Tuple2 [C, S]) =>
        acc + ((sample, if (acc.contains(sample)) { acc(sample) + 1 } else { 1 }))
    }

    new NaiveBayesClassifier(
      classes.foldLeft(Map[C, Double]()) {
        (acc : Map[C, Double], entry : Tuple2[C, Double]) => acc + ((entry._1, entry._2 / samples.size))
      },
      frequences.foldLeft(Map[Tuple2 [C, S], Double]()) {
        (acc : Map[Tuple2[C, S], Double], entry : Tuple2[Tuple2[C, S], Double]) =>
          acc + ((entry._1, entry._2 / classes(entry._1._1)))
      })
  }
}