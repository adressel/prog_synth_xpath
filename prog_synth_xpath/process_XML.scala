package prog_synth_xpath
import scala.io.Source
import scala.xml.XML
import scala.xml.Node
import scala.collection.mutable.MutableList

class input_node (
  val output_node : output_node,
  val DTD_id : Int,
  val children : MutableList[Int] = MutableList()
) extends Variable {}

class output_node (
    
) extends Variable {}

object process_XML{
	var xml_map:Map[String, input_node] = Map()
	var child_parent:Map[Int, Int] = Map()
	def all = xml_map
	def child_to_parent = child_parent
	
	def populate = {
	  	val tempNode = new input_node(new output_node, 1)
	  	child_parent += (2 -> 0)
	   	xml_map += (DATA.xml.descendant_or_self(0).toString -> tempNode)
	  	populate_helper(DATA.xml.descendant_or_self(0), tempNode)
	}
	
	def populate_helper(node : Node, node_info : input_node) : Unit = {
	  if (node.descendant.length == 1) return 
	  for (descendant_node <- node.child) {
		  val tempNode = new input_node (new output_node, DTD.all(descendant_node.label))
		  child_parent += (tempNode.id -> node_info.id)
//		  println(tempNode.id +"   "+ node_info)
		  node_info.children += tempNode.id
		  xml_map += (descendant_node.toString -> tempNode)
		  populate_helper(descendant_node, tempNode)
	  }
	}
}











