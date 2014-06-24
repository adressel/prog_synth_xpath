package prog_synth_xpath
import scala.io.Source

import scala.xml.XML

object process_XML extends App {
	val asdf = XML.loadFile("./xml/DatabaseInventory.xml")
	
	println(XML.mkString)
	
}