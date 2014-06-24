package od.andrey.common

/**
 * Created by Alemeshev on 23.06.2014.
 */
trait Classifier [C, S] {
  def classify(features : List[S]): C
}
