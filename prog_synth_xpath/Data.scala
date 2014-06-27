package prog_synth_xpath

import scala.xml.XML

object Data {
//	val root = "." // for Ian
    val root = "/Users/Stephen/scalaWorkSpace/xpath_T/weka/" // for Sheng
  
	val xml_xpath = "DatabaseName"
	val xml = XML.loadFile(s"$root/xml/DatabaseInventory.xml")
}
