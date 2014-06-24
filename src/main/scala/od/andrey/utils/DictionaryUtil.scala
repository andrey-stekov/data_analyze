package od.andrey.utils

import scala.io.BufferedSource
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: ALemeshev
 * Date: 23.05.14
 * Time: 14:41
 * To change this template use File | Settings | File Templates.
 */
object DictionaryUtil {
  val EXACT_MATCHING : Double = 1.0

  def getNGrammsBySize(word : String, size : Int):Map[String, Double] = {
    if (word.size <= size) {
      return Map((word, EXACT_MATCHING))
    }

    val wordSize = word.size.toDouble

    (0 to word.size - size - 1).foldLeft(Map[String, Double]())(
      (map : Map[String, Double], index : Int) => {
        map + ((word.substring(index, index + size), size.toDouble / wordSize))
      }) + ((word, EXACT_MATCHING))
  }

  def getNGramms(word : String, minSize : Int):Map[String, Double] = {
    if (word.size <= minSize) {
      return Map((word, EXACT_MATCHING))
    }

    (minSize to word.size - 1).foldLeft(Map[String, Double]())(
      (map : Map[String, Double], size : Int) => {
        map ++ getNGrammsBySize(word, size) // TODO: Fix it
      }) + ((word, EXACT_MATCHING))
  }

  def getWordsFromText(s : BufferedSource) :Iterable[String] = {
    var tmp  : Set[String] = Set[String]()
    val word : mutable.StringBuilder = mutable.StringBuilder.newBuilder

    for (c <- s.toIterator) {
      if (!c.isLetter) {
        if (!word.isEmpty) {
          tmp = tmp + word.mkString
          word.clear()
        }
      } else {
        word.append(c)
      }
    }

    tmp
  }

  def getDictionary(words : Iterable[String],
                    strategy : (String, Map[String, Double]) => (String, Double) ) : Map[String, Double] = {
    words.foldLeft(Map[String, Double]())(
      (map : Map[String, Double], word : String) => {
        map + strategy(word, map)
      })
  }
}
