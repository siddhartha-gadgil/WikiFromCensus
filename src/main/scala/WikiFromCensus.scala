import scala.util._

import scala.io.Source

import java.io._

object WikiFromCensus {
  lazy val src = Source.fromFile("data/Gadchiroli.csv")

  lazy val lines = src.getLines().toList

  lazy val rawtab = lines map (_.split(",").toList)

  def isInt(s : String) = !Try(s.toInt).toOption.isEmpty

  def isVillage(l: List[String]) = isInt(l(0)) && !isInt(l(1))

  val tab = rawtab filter (isVillage)

  object Number{
    def unapply(s : String) = Try(s.toInt).toOption
  }

  def available(s: String, desc: String) =
    if (s.toLowerCase == "yes") s"$desc facility is available in the village"
      else s"$desc facility is not available in the village"

  def abc(s : String) = s match {
    case "a" => "< 5 km"
    case "b" => "5-10 kms"
    case "c" => "> 10 km"
    case _ => "???"
  }

  def numOrNearest(s: String, desc: String) = s match {
    case Number(k) => s"There are $k $desc in the village"
    case _ => s"The nearest $desc is at a distance of ${abc(s)} from the village."
  }

  def numOrBlank(s: String, desc: String) = s match {
    case Number(k) => s"There are $k $desc in the village"
    case _ => ""
  }

  def hasOrNearest(s: String, desc: String) = s.toLowerCase match {
    case "yes" => s"$desc facility is available in the village"
    case _ => s"The nearest $desc facility is at a distance of ${abc(s)} from the village."
  }

  def allPages = tab map (page)

  def manuf(row: List[String]) = {
    val items = List(row(119), row(120), row(121)) filter (_ != "")
    if (items.isEmpty) ""
      else
        s"""${row(1)} is engaged in the manufacture of following items (in decreasing order of importance): ${items.mkString(",")}"""
  }

  def savePage(row: List[String]) = {
    val filename = s"data/${row(1)}.txt"
    val f = new PrintWriter(filename)
    f.println(page(row))
    f.close()
  }


  def page(row : List[String]) =
    s"""
${row(1)} is a village in the Gadchiroli  with an area of ${row(3)} hectares, harbouring ${row(5)} households and with total population of ${row(4)} as per the 2011 Census. The nearest town is ${row(101)} at a distance of ${abc(row(102))}.

Educational facilities

${numOrNearest(row(6), "Pre-primary schools")}
${numOrNearest(row(7), "Primary schools")}
${numOrNearest(row(8), "Middle schools")}
${numOrNearest(row(9), "Secondary schools")}
${numOrNearest(row(10), "Senior secondary schools")}

${numOrBlank(row(19), "other educational institutes")}

Drinking water
${available(row(38), "Drinking water from taps")}
${available(row(39), "Drinking water from wells")}

Communication and transport
${hasOrNearest(row(50), "Post office")}
${hasOrNearest(row(51), "Sub Post office")}

Land use
${row(1)} exhibits the following land use pattern (area in hectares):
Forests: ${row(103)}
Area under Non-agricultural Uses : ${row(104)}
Barren and Un-cultivable land:${row(105)}
Permanent Pastures and Other Grazing Lands:${row(106)}
Land Under Miscellaneous Tree Crops etc. :${row(107)}
Culturable Waste Land:${row(108)}
Fallow lands other than current fallows:${row(109)}
Current Fallows:${row(110)}
Net Area Sown:${row(111)}
Total Irrigated Land Area:${row(112)}
Total Un-irrigated Land Area:${row(113)}
Irrigation facilities
Sources of irrigation are as follows (area in hectares):
Canals: ${row(114)}
Wells/Tube-wells: ${row(115)}
Tanks/Lakes: ${row(116)}
Water Falls: ${row(117)}
Others: ${row(118)}

Manufacture
${manuf(row)}
                  """

}
