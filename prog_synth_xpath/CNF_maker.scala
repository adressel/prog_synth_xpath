package prog_synth_xpath
import scala.collection.mutable.ArrayBuffer
import java.lang.StringBuilder
import scala.collection._

object CNF_maker {
	var count = 0;
	val clauses : mutable.ArrayBuffer[mutable.ArrayBuffer[String]] = mutable.ArrayBuffer()
	
	def rule_1 = {
		val clause_buffer_1 : mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
		val input_xml_map = process_XML.all
		for (eachNode <- input_xml_map){
			var tempClauseString : String= "-" + eachNode._2.output_node.id + " " + eachNode._2.id
			var tempNodeId = eachNode._2.id
			while (process_XML.child_to_parent(tempNodeId) != 0){
				tempNodeId = process_XML.child_to_parent(tempNodeId) 
				tempClauseString ++= " " + tempNodeId
			}
			clause_buffer_1 += tempClauseString
		}
		count += clause_buffer_1.length
		clauses += clause_buffer_1
	}
	
	val clause_buffer_2 : ArrayBuffer[String] = ArrayBuffer()
	val clause_buffer_3 : ArrayBuffer[String] = ArrayBuffer()
	def rule_23 = {
	  val node_sv_map : mutable.Map[input_node, ArrayBuffer[Int]] = mutable.Map()
	  
	  for(sv <- SelectVariable.all) {
	    val matched_nodes = sv.matched_nodes
	    val unmatched_nodes = (Data.xml \\ sv.label).toSet -- matched_nodes.toSet
	    for(node <- matched_nodes) {
	      val node_id = process_XML.xml_map(node.text).output_node.id
	      clause_buffer_3 += s"-${sv.id} $node_id"
	      node_sv_map(process_XML.xml_map(node.text)) += sv.id
	    }
	    for(node <- unmatched_nodes) {
	      val node_id = process_XML.xml_map(node.text).output_node.id
	      clause_buffer_3 += s"-${sv.id} -$node_id"
	    }
	  }
	  
	  for((node, sv_ids) <- node_sv_map) {
	    var clause_string = s"-${node.output_node.id}"
	    if(sv_ids.length > 0)
	      clause_string += s" ${sv_ids.mkString(" ")}"
	    if(node.children.length > 0) {
	    	val child_output_id = for (child_id <- node.children) yield{
	    		process_XML.xml_map.filter(_._2.id == child_id).toList(0)._2.output_node.id
	    	}
	    }
//	      NEED CHILDREN'S OUTPUT_NODE IDS
	  }
	}
}