package prog_synth_xpath
import scala.xml._
import scala.collection.mutable.ArrayBuffer
import scala.collection._

class Label (val label : String, val node_info_Goodids: Vector[Int], val node_info_Badids: Vector[Int]) {
  populate
  
  def populate = {
    this.NodeVariable.populate
    this.SelectVariable.populate
  }
  
  object NodeVariable {
    private var nvs : ArrayBuffer[NodeVariable] = ArrayBuffer()
    val id_map : mutable.Map[Int, Int] = mutable.Map()
    def all = nvs
    def populate = {
      //construct NodeVariables from NodeInfos with matching labels
      for(node_info <- NodeInfo.all.filter(_.label == label)) {
        if (node_info_Goodids.contains(node_info.id) || node_info_Badids.contains(node_info.id)){
	        val nv = new NodeVariable(node_info)
	        id_map += (node_info.id -> nv.id)
	        println(s"node_info_id : ${node_info.id}   node_V id : ${nv.id}")
	        nvs += nv
        }
      }
    }
  }
  
  class NodeVariable (
    val info : NodeInfo
  ) extends Variable {
    val output_id = Variable.get_id
    Variable.id_map(output_id) = this
    val sv_match_ids : ArrayBuffer[Int] = ArrayBuffer()
  }
  
  object SelectVariable {
    var svs: ArrayBuffer[SelectVariable] = ArrayBuffer()
    def all = svs

    def add_attribute_variables(attr: MetaData) {
//      svs += new AttributeVariable(attr.key, attr.value.text, "=")
      svs += new AttributeVariable(attr.key, attr.value.text, "<=")
      svs += new AttributeVariable(attr.key, attr.value.text, ">=")
    }

    def add_text_variables(text: String) {
//      svs += new TextVariable(text, "=")
      svs += new TextVariable(text, "<=")
      svs += new TextVariable(text, ">=")
    }

    def populate = {
      val nv_ids = node_info_Goodids.map(NodeVariable.id_map(_))
      var sv_buffer: ArrayBuffer[SelectVariable] = ArrayBuffer()
      for (node <- NodeVariable.all.filter(s => nv_ids.contains(s.id))) {
        println(node.info.text)
        for (attr <- node.info.attrs)
          add_attribute_variables(attr)
          
        if (node.info.text != "")
          add_text_variables(node.info.text)
      }
    }
  }

  class AttributeVariable(
    key: String,
    value: String,
    operator: String
    ) extends SelectVariable(label) {
    def matched_nodes = {
      operator match {
//        case "=" => scope.filter(_.info.attrs(key).toString == value)
        case "<=" => scope.filter(_.info.attrs(key).toString <= value)
        case ">=" => scope.filter(_.info.attrs(key).toString >= value)
      }
    }
    override def toString = s"$label.$key $operator $value"
    override def expression = s"@${key}${operator}'${value}'"
  }

  class TextVariable(
    text: String,
    operator: String
    ) extends SelectVariable(label) {

    def matched_nodes = {
      operator match {
//        case "=" => scope.filter(_.info.text == text)
        case ">=" => scope.filter(_.info.text >= text)
        case "<=" => scope.filter(_.info.text <= text)
      }
    }
    override def toString = s"<$label>${operator}$text"
    override def expression = s"text() $operator '$text'"
  }

  abstract class SelectVariable(
    val label: String
    ) extends Variable {
    // matched_nodes keeps track of which nodes are mapped to which
    def scope = NodeVariable.all
    def matched_nodes: ArrayBuffer[NodeVariable]
    def expression: String
  }

  class Variable {
    val id = Variable.get_id
    Variable.id_map(id) = this
  }

  object Variable {
    val id_map: mutable.Map[Int, Variable] = mutable.Map()
    var count = 0
    def get_id = {count += 1; count}
  }
}
