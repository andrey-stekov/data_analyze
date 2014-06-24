package od.andrey.spider

import java.net.URL
import org.jsoup.nodes.Document

/**
 * Created by ALemeshev on 05.06.2014.
 */
object Stub {
  def main(args: Array[String]) {
    val settings = new ProxySettings("ODS-PROXY.KIEV.LUXOFT.COM", "8080", "KIEV\\ALemeshev", "fv,'h2014")
    val spider = new Spider[String](settings, settings)
    val url = "http://go.mail.ru/search?mailru=1&q=%D0%BA%D0%BE%D1%88%D0%B0%D1%87%D1%8C%D0%B8+%D1%83%D1%88%D0%BA%D0%B8&us=9&usln=1"

    val res = spider.walk(url, 2, (url : String, doc : Document) => {
      doc.title()
    })

    println(res)
  }
}
