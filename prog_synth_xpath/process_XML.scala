package prog_synth_xpath
import scala.io.Source

import scala.xml.XML

object process_XML{
	var xml_map:Map[String, Tuple2[Int, Int]] = Map()
	var total_id = 0
	
	def populate = {
		for (dtd_element <- DTD.all){
			val temp =  DATA.xml \\ dtd_element._1 
			var node_id = 0
			for(xml_node <- temp){
			  total_id += 1
			  node_id += 1
			  xml_map += (xml_node.toString -> (dtd_element._2, node_id))
			}
		}
//		xml_map.map(s => println(s._1 + " " + s._2))
	}
}











