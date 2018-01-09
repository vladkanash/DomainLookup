import java.io.File
import scala.io.Source
import scala.util.matching.Regex

val baseRegexp = """:/+.*?(/|"|$|\)|')"""
val domainRegexpHttp = ("http" + baseRegexp).r
val domainRegexpHttps = ("https" + baseRegexp).r
val encoding = "ISO-8859-1"
val extensions = args(1).split("/")
val dir = new File(args(0))
val separator = "======================================================================================="
val outputFileName = "links.txt"

def getStats(dir: File,
             extension: String,
             func: File => TraversableOnce[String]) = {

  def processDirs(dir: File): Array[String] = {
    val (dirs, files) = dir.listFiles.partition(_.isDirectory)
    val fileLinks = files
      .filter(file => file.getName.endsWith(extension))
      .flatMap(func)
    fileLinks ++ dirs.flatMap(processDirs)
  }

  processDirs(dir)
    .groupBy(identity)
    .mapValues(_.length)
    .toSeq
    .sortBy(_._1)
}

def getFileLinks(regexp: Regex)(file: File) =
  Source
    .fromFile(file, encoding)
    .getLines
    .flatMap(regexp.findAllIn(_))

def printTuples(tuples: Iterable[(_, _)]) =
  tuples
    .map(_.productIterator.mkString(" -> "))
    .foreach(println)

def printLinkList(httpLinks: Iterable[(_,_)], 
                  httpsLinks: Iterable[(_,_)], 
                  ext: String) {
  println("HTTP LINKS FOR " + ext + " :")
  printTuples(httpLinks)
  println("\nHTTPS LINKS FOR " + ext + " :")
  printTuples(httpsLinks)
  println(separator + "\n")
}

if (!(dir.exists && dir.isDirectory)) {
  println("pls provide a path to actual directory")
  System.exit(0)
}

extensions.foreach(ext =>
  printLinkList(getStats(dir, ext, getFileLinks(domainRegexpHttp)), 
                getStats(dir, ext, getFileLinks(domainRegexpHttps)),
                ext))

// val total = httpLinks.map(_._2).sum + httpsLinks.map(_._2).sum
// println("\nTotal: " + total)
