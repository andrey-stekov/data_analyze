package od.andrey.spider

import org.jsoup.Jsoup
import java.net.{URLDecoder, URL}
import org.jsoup.nodes.{Document, Element}
import scala.collection.JavaConverters._

/**
 * Created by ALemeshev on 05.06.2014.
 */
class Spider[R] (httpProxy : ProxySettings, httpsProxy : ProxySettings) {
  if (httpProxy != null) {
    System.setProperty("http.proxyHost", httpProxy.host)
    System.setProperty("http.proxyPort", httpProxy.port)
    System.setProperty("http.proxyUser", httpProxy.login)
    System.setProperty("http.proxyPassword", httpProxy.password)
  }

  if (httpsProxy != null) {
    System.setProperty("https.proxyHost", httpsProxy.host)
    System.setProperty("https.proxyPort", httpsProxy.port)
    System.setProperty("https.proxyUser", httpsProxy.login)
    System.setProperty("https.proxyPassword", httpsProxy.password)
  }

  def getURLs(domain:String, doc : Document) : List[String] = {
    val protocol = new URL(domain).getProtocol

    doc.body().select("a").iterator().asScala.foldLeft(List[String]()){
      (accumulator : List[String], anchor : Element) =>
        val href = anchor.attr("href")
        if (href.startsWith("javascript") || href.startsWith("#")) {
          accumulator
        } else {
           if (href.startsWith("//")) {
             accumulator :+ protocol + ":" + href
           } else if (href.startsWith("/")) {
             accumulator :+ domain + href
           } else if (href.startsWith("?")) {
             accumulator :+ domain + "/" + href
           } else {
             val pos = href.lastIndexOf("http")

             if (pos < 0) {
               accumulator
             } else if (pos == 0) {
               accumulator :+ href
             } else {
               if (href.indexOf("http:", pos) > 0) {
                 accumulator :+ href :+ href.substring(pos)
               } else if (href.indexOf("http%3A", pos) > 0) {
                 accumulator :+ URLDecoder.decode(href.substring(pos), "UTF-8")
               } else {
                 accumulator
               }
             }
           }
        }
      }
  }

  def walk(url : String, deep : Int, func : (String, Document) => R) : Map[String, R] = {
    if (deep == 0) {
      return Map.empty
    }

    try {
      val doc : Document = Jsoup.parse(new URL(url), 1000)
      val domain:String = "http[s]?:\\/\\/[^\\/]+(?=\\/)".r.findFirstIn(url).getOrElse("http[s]?:\\/\\/[^\\/]+".r.findFirstIn(url).get)

      getURLs(domain, doc).foldLeft(Map((url.toString, func(url, doc)))) {
        (accumulator: Map[String, R], url: String) =>
          if (url.isEmpty) {
            accumulator
          } else {
            accumulator ++ walk(url, deep - 1, func)
          }
      }
    } catch {
      case e : Exception =>
        println(url + ", " + e)
        Map.empty
    }


  }

}
