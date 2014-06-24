package prog_synth_xpath
//import scala.io.Source

import scala.xml.XML

object XML extends App {
	val XML = XML.loadFile("./xml/DatabaseInventory.xml")
	
	println(XML.mkString)
	
}