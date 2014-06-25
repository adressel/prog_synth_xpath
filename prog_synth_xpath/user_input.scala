package prog_synth_xpath
import scala.collection._

object user_input {
	val xml_user_id_list : mutable.ArrayBuffer[Tuple2[Int, Int]] = mutable.ArrayBuffer()
	val xml_user_node_list : mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

	def prase_user_nodes = {
	  val user_nodes =  DATA.xml \\ DATA.xml_xpath
	  for (user_node <- user_nodes){
	    xml_user_node_list += user_node.toString
	    println (user_node.toString)
	  }
//	  xml_user_node_list.map(s => println(s))
	}
	
	def get_userNode_id = {
		for (user_node <- xml_user_node_list){
		  if (process_XML.xml_map.contains(user_node))
			 xml_user_id_list += process_XML.xml_map(user_node)
		}
//		xml_user_id_list.map(s => println(s))
	}
}