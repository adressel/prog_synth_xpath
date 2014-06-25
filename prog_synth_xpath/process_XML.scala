package prog_synth_xpath
import scala.io.Source

import scala.xml.XML

//class xml_node (
//	val contents : String,
//	val xml_id : Int,
//	val dtd_id : Int 
//)

object process_XML{
	var xml_map:Map[String, Tuple2[Int, Int]] = Map()
	var total_id = 0
	
	def populate = {
		for (dtd_element <- DTD.dtd_map){
			val temp =  DATA.xml \\ dtd_element._1 
			for(xml_node <- temp){
			  total_id += 1
			  xml_map += (xml_node.toString -> (dtd_element._2, total_id))
			}
		}
//		xml_map.map(s => println(s._1 + " " + s._2))
	}
}











