package prog_synth_xpath

import scala.collection._
import scala.xml._

class NodeInfo(
  val label: String,
  val attrs: MetaData,
  val parent_id: Int) {
  var children_ids: Vector[Int] = Vector()
  var text: String = ""
  val id: Int = NodeInfo.get_id
}

object NodeInfo {
  private var id = 0;
  def get_id = { id += 1; id - 1 }
  private val node_infos: mutable.ArrayBuffer[NodeInfo] = mutable.ArrayBuffer()
  def all = node_infos
  def populate = populate_helper(Data.xml, 0)

  def populate_helper(node: Node, parent_id: Int): Int = {
    val node_info = new NodeInfo(node.label, node.attributes, parent_id)
    node_info.text = node.text
    if (node.descendant.length == 1) {
      node_infos += node_info
      return node_info.id
    }

    val children_ids = node.descendant.map(populate_helper(_, node_info.id))
    
    node_info.children_ids = children_ids.toVector
    node_infos += node_info
    node_info.id
  }
}