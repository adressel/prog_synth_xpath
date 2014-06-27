package prog_synth_xpath
import scala.collection.mutable._
import java.lang.StringBuilder
import scala.collection._

object CNF_maker {
	var count = 0;
	val clauses : mutable.ArrayBuffer[mutable.ArrayBuffer[String]] = mutable.ArrayBuffer()
	
	def rule_1 = {
		val clause_buffer_1 : mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
		val input_xml_map = process_XML.all
		for (nodes <- input_xml_map){
		  for (eachNode <- nodes._2){
		    clause_buffer_1 += s"-${eachNode.id} ${eachNode.output_node.id}"
			var tempClauseString = s"-${eachNode.output_node.id} ${eachNode.id}"
			var tempNodeId = eachNode.id
			while (process_XML.child_to_parent(tempNodeId) != 0){
				tempNodeId = process_XML.child_to_parent(tempNodeId) 
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
	  val node_sv_map : mutable.Map[input_node, ArrayBuffer[Int]] = mutable.Map()
	  
	  for(sv <- SelectVariable.all) {
	    //matched_nodes and unmatched_nodes are both sequences that list the nodes
	    //that sv "selects"
	    val matched_nodes = sv.matched_nodes.toSet
//	    		println("\nmathced_node: ")
//	    	matched_nodes.map(s => println(s +" " + process_XML.xml_map(s.toString).output_node.id))
	    	
	    val unmatched_nodes = (Data.xml \\ sv.label).toSet -- matched_nodes
//	    	println("\nunmathced_node: ")
//	    	unmatched_nodes.map(s => println(s +" " + process_XML.xml_map(s.toString).output_node.id))
	    for(node <- matched_nodes) {
	      // get node_id from node.text (super inefficient right now)
	      for (input_node <- process_XML.xml_map(node.toString))
	      {
	    	  val id = input_node.output_node.id
		      clause_buffer_3 += s"-${sv.id} ${id}"
//		      println(s"-${sv.id} $id ${sv.toString()}")
		      // update node_sv_map
		      if (node_sv_map.contains(input_node))
		    	  node_sv_map(input_node) += sv.id
		      else node_sv_map += (input_node -> ArrayBuffer(sv.id))
	      }
	    }
	    
	    for(node <- unmatched_nodes) {
	      for (input_node <- process_XML.xml_map(node.toString)){
		      clause_buffer_3 += s"-${sv.id} -${input_node.output_node.id}"
		    }
	    }
	  }
//	  println("start rule 2")
	  // RULE 2
	  for((node, sv_ids) <- node_sv_map) {
	    var clause_string = s"-${node.output_node.id}"
	    if(sv_ids.length > 0)
	      clause_string += s" ${sv_ids.mkString(" ")}"
	    if(node.children.length > 0) {
	      val child_output_id : MutableList[Int] = new MutableList() 
	      for (child_id <- node.children) {
	        for ((_,input_node) <- process_XML.xml_map){
	          child_output_id += input_node.filter(_.id == child_id).toList(0).output_node.id
	        }
	      }
	      clause_string += s" ${child_output_id.mkString(" ")}"
	    }
	    clause_buffer_2 += clause_string
	  }
	  clauses += clause_buffer_2
	  clauses += clause_buffer_3
	}
}










