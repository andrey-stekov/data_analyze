package od.andrey.analyze.text

import od.andrey.bayes.NaiveBayesClassifier
import java.io.InputStream

/**
 * Created by Alemeshev on 23.06.2014.
 */
class TextClassifierBuilder {
  def buildClassifier[C](samples : List[Tuple2[C, InputStream]]) : NaiveBayesClassifier[C, String] = {
    null
  }

  private def buildClassifier2[C, S](samples : List[Tuple2 [C, S]]) : NaiveBayesClassifier[C, S] = {
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
