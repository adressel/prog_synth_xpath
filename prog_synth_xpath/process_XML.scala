package prog_synth_xpath
import scala.io.Source
import scala.xml.XML
import scala.xml.Node
import scala.collection.mutable.MutableList

class input_node (
  val output_node : output_node,
  val children : MutableList[Int] = MutableList(),
  var real_user_input : Boolean = false 
) extends Variable {
  override def toString() = {
	 if (real_user_input) id + " 0\n"
	 else "-" + id + " 0\n"
  }
}

class output_node (
    
) extends Variable {
}

object process_XML{
	var xml_map:Map[String, MutableList[input_node]] = Map()// text -> input_node
	var child_parent:Map[Int, Int] = Map()// child_id -> parent_id
	def all = xml_map
	def child_to_parent = child_parent
	
	def populate = {
	  	val tempNode = new input_node(new output_node)
	  	child_parent += (2 -> 0)
	  	if (xml_map.contains(Data.xml.descendant_or_self(0).toString))
	  	  xml_map(Data.xml.descendant_or_self(0).toString) += tempNode
	  	else xml_map += (Data.xml.descendant_or_self(0).toString -> MutableList(tempNode))
	  	populate_helper(Data.xml.descendant_or_self(0), tempNode)
	}
	
	def populate_helper(node : Node, node_info : input_node) : Unit = {
//	  println(node.text + "  " + node_info.id)
	  if (node.descendant.length == 1) return
	  for (descendant_node <- node.child) {
		  val tempNode = new input_node (new output_node)
		  child_parent += (tempNode.id -> node_info.id)
		  node_info.children += tempNode.id
		  if (xml_map.contains(descendant_node.toString))
			  xml_map(descendant_node.toString) += tempNode
	  	  else xml_map += (descendant_node.toString -> MutableList(tempNode))
		  populate_helper(descendant_node, tempNode)
	  }
	}
}











