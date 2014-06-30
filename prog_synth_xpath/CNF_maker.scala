package prog_synth_xpath
import scala.collection.mutable._
import java.lang.StringBuilder
import scala.collection._

object CNF_maker {
	var count = 0;
	val clauses : mutable.ArrayBuffer[mutable.ArrayBuffer[String]] = mutable.ArrayBuffer()
	
	def rule_1 = {
		val clause_buffer_1 : mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
		val input_xml_map = XMLNode.all
		for (nodes <- input_xml_map){
		  for (node <- nodes._2){
		    clause_buffer_1 += s"-${node.input_id} ${node.output_id}"
			var tempClauseString = s"-${node.output_id} ${node.input_id}"
			var tempNodeId = node.input_id
			while (XMLNode.child_to_parent(tempNodeId) != 0){
				tempNodeId = XMLNode.child_to_parent(tempNodeId) 
				tempClauseString ++= " " + tempNodeId
			}
			clause_buffer_1 += tempClauseString
		  }
		}
		count += clause_buffer_1.length
		clauses += clause_buffer_1
	}
	
	val clause_buffer_2 : ArrayBuffer[String] = ArrayBuffer()
	val clause_buffer_3 : ArrayBuffer[String] = ArrayBuffer()
	def rule_23 = {
	  //node_sv_map keeps track of which SV's map to which ON's
	  val node_sv_map : mutable.Map[XMLNode, ArrayBuffer[Int]] = mutable.Map()
	  
	  for(sv <- SelectVariable.all) {
	    //matched_nodes and unmatched_nodes are both sequences that list the nodes
	    //that sv "selects"
	    val matched_nodes = sv.matched_nodes.toSet
	    	
	    val unmatched_nodes = (Data.xml \\ sv.label).toSet -- matched_nodes
	    for(nodes <- matched_nodes) {
	      for (node <- XMLNode.xml_map(nodes.toString))
	      {
	    	  val id = node.output_id
		      clause_buffer_3 += s"-${sv.id} ${id}"
		      // update node_sv_map
		      if (node_sv_map.contains(node))
		    	  node_sv_map(node) += sv.id
		      else node_sv_map += (node -> ArrayBuffer(sv.id))
	      }
	    }
	    
	    for(nodes <- unmatched_nodes) {
	      for (node <- XMLNode.xml_map(nodes.toString)){
		      clause_buffer_3 += s"-${sv.id} -${node.output_id}"
		    }
	    }
	  }
//	  println("start rule 2")
	  // RULE 2
	  for((node, sv_ids) <- node_sv_map) {
	    var clause_string = s"-${node.output_id}"
	    if(sv_ids.length > 0)
	      clause_string += s" ${sv_ids.mkString(" ")}"
	    if(node.children.length > 0) {
	      val child_output_ids = node.children.map(x => x.output_id)
	      clause_string += s" ${child_output_ids.mkString(" ")}"
	    }
	    clause_buffer_2 += clause_string
	  }
	  clauses += clause_buffer_2
	  clauses += clause_buffer_3
	}
}










