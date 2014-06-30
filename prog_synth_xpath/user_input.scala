package prog_synth_xpath
import scala.collection._

object user_input {
	val xml_user_id_list : mutable.ArrayBuffer[input_node] = mutable.ArrayBuffer()
	val xml_user_node_list : mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

	def parse_user_nodes = { // according to the user xpath to get the nodes user expected 
	  val user_nodes =  Data.xml \\ Data.xml_xpath
//	  for (user_node <- user_nodes)
	    xml_user_node_list += user_nodes(0).toString
	    println("input string : " + user_nodes(0).toString)
	}
	
	def get_userNode_id = { // according to the xml_user_node_list we can get the id for each node
		for (user_node <- xml_user_node_list){
		  if (process_XML.xml_map.contains(user_node)){
		    var isDone = false 
		    for (input <- process_XML.xml_map(user_node)){
		      if (!isDone && !input.real_user_input){
			 	xml_user_id_list += input
			 	input.real_user_input = true
			 	isDone = true
		      }
		    }
		  }
		}
	}
}