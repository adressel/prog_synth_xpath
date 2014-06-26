package prog_synth_xpath
import scala.collection.mutable.ArrayBuffer
import java.lang.StringBuilder
import scala.collection._

object CNF_maker {
	val clause_buffer_1 : mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
	def rule_1 = {
		val input_xml_map = process_XML.all
		for (eachNode <- input_xml_map){
			var tempClauseString : String= "-" + eachNode._2.output_node.id + " " + eachNode._2.id
			var tempNodeId = eachNode._2.id
//			 println("parent id: " + process_XML.child_to_parent(tempNodeId) + " " + tempNodeId)
			while (process_XML.child_to_parent(tempNodeId) != 0){
				tempNodeId = process_XML.child_to_parent(tempNodeId) 
				tempClauseString ++= " " + tempNodeId
			}
			clause_buffer_1 += tempClauseString
		}
	}
}