package od.andrey.utils

import scala.io.Source
import od.andrey.bayes.NaiveBayesClassifier

/**
 * Created by ALemeshev on 21.06.2014.
 */
object Util {
  def getFeature(s : String) : String = {
    s.last.toString
  }

  def loadNamesDict(file : String) : List[Tuple2[String, String]] = {
    Source.fromInputStream(Thread.currentThread().getContextClassLoader.getResourceAsStream(file)).getLines()
      .foldLeft(List[Tuple2[String, String]]()) {
      (acc : List[Tuple2[String, String]], line : String) =>
        val tmp : Array[String] = line.trim.split("[\\s]+")
        acc :+ ((tmp(1), getFeature(tmp(0))))
    }
  }

  def testCase(classifier : NaiveBayesClassifier[String, String], name : String) {
    println(name + ", " + classifier.classify(List(getFeature(name))))
  }

  def test() {
    val samples : List[Tuple2[String, String]] = loadNamesDict("names.txt")
    val classifier : NaiveBayesClassifier[String, String] = NaiveBayesClassifier.buildClassifier[String, String](samples)

    testCase(classifier, "Эоиу")
    testCase(classifier, "Оаиа")
  }
}
