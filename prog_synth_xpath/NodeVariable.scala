package prog_synth_xpath
import scala.io.Source
import scala.xml.XML
import scala.xml.Node
import scala.collection.mutable._

class NodeVariable (
  val input_id : Int = Variable.get_id,
  val output_id : Int = Variable.get_id,
  val children : ArrayBuffer[NodeVariable] = ArrayBuffer(),
  var real_user_input : Boolean = false 
) {
  override def toString() = {
	 if (real_user_input) input_id + " 0\n"
	 else "-" +input_id + " 0\n"
  }
}

object NodeVariable{
	var xml_map:Map[Node, ArrayBuffer[NodeVariable]] = Map()// text -> NodeVariable
	var child_parent:Map[Int, Int] = Map()// child_id -> parent_id
	
	def all = xml_map
	def child_to_parent = child_parent

	def populate = {
	  	val temp_node = new NodeVariable
	  	child_parent += (temp_node.input_id  -> 0)
	  	if (xml_map.contains(Data.xml.descendant_or_self(0)))
	  	  xml_map(Data.xml.descendant_or_self(0)) += temp_node
	  	else xml_map += (Data.xml.descendant_or_self(0) -> ArrayBuffer(temp_node))
	  	populate_helper(Data.xml.descendant_or_self(0), temp_node)
	}
	
	def populate_helper(node : Node, node_info : NodeVariable) : Unit = {
	  if (node.descendant.length == 1) return
	  for (descendant_node <- node.child) {
		  val temp_node = new NodeVariable
		  child_parent += (temp_node.input_id -> node_info.input_id)
		  node_info.children += temp_node
		  if (xml_map.contains(descendant_node))
			  xml_map(descendant_node) += temp_node
	  	  else xml_map += (descendant_node -> ArrayBuffer(temp_node))
		  populate_helper(descendant_node, temp_node)
	  }
	}
}











