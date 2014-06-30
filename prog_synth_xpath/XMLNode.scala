package prog_synth_xpath
import scala.io.Source
import scala.xml.XML
import scala.xml.Node
import scala.collection.mutable._

class XMLNode (
  val input_id : Int = Variable.get_id,
  val output_id : Int = Variable.get_id,
  val children : ArrayBuffer[XMLNode] = ArrayBuffer(),
  var real_user_input : Boolean = false 
) {
  override def toString() = {
	 if (real_user_input) input_id + " 0\n"
	 else "-" +input_id + " 0\n"
  }
}

object XMLNode{
	var xml_map:Map[String, MutableList[XMLNode]] = Map()// text -> XMLNode
	var child_parent:Map[Int, Int] = Map()// child_id -> parent_id
	def all = xml_map
	def child_to_parent = child_parent
	
	def populate = {
	  	val tempNode = new XMLNode
	  	child_parent += (tempNode.input_id  -> 0)
	  	if (xml_map.contains(Data.xml.descendant_or_self(0).toString))
	  	  xml_map(Data.xml.descendant_or_self(0).toString) += tempNode
	  	else xml_map += (Data.xml.descendant_or_self(0).toString -> MutableList(tempNode))
	  	populate_helper(Data.xml.descendant_or_self(0), tempNode)
	}
	
	def populate_helper(node : Node, node_info : XMLNode) : Unit = {
	  if (node.descendant.length == 1) return
	  for (descendant_node <- node.child) {
		  val temp_node = new XMLNode
		  child_parent += (temp_node.input_id -> node_info.input_id)
		  node_info.children += temp_node
		  if (xml_map.contains(descendant_node.toString))
			  xml_map(descendant_node.toString) += temp_node
	  	  else xml_map += (descendant_node.toString -> MutableList(temp_node))
		  populate_helper(descendant_node, temp_node)
	  }
	}
}











