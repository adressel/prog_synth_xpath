package prog_synth_xpath

import scala.xml.XML

object DATA {
	val root = "./" // for Ian
//    val root = "/Users/Stephen/Desktop/prog_synth_sql/" // for Sheng
  
	val xml_xpath = "DatabaseName"
	val xml = XML.loadFile("/Users/Stephen/scalaWorkSpace/xpath_T/weka/xml/DatabaseInventory.xml")
}
