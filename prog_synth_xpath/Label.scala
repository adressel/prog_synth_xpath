package prog_synth_xpath
import scala.xml._
import scala.collection.mutable.ArrayBuffer
import scala.collection._

class Label (val label : String) {
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
        val nv = new NodeVariable(node_info)
        id_map += (node_info.id -> nv.id)
        nvs += nv
      }
    }
  }
  
  class NodeVariable (
    val info : NodeInfo
  ) extends Variable {
    val sv_match_ids : ArrayBuffer[Int] = ArrayBuffer()
  }
  
  object SelectVariable {
    var svs: ArrayBuffer[SelectVariable] = ArrayBuffer()
    def all = svs

    def add_attribute_variables(attr: MetaData) {
      svs += new AttributeVariable(attr.key, attr.value.text, "=")
      svs += new AttributeVariable(attr.key, attr.value.text, "<=")
      svs += new AttributeVariable(attr.key, attr.value.text, ">=")
    }

    def add_text_variables(text: String) {
      svs += new TextVariable(text, "=")
      svs += new TextVariable(text, "<=")
      svs += new TextVariable(text, ">=")
    }

    def populate = {
      var sv_buffer: ArrayBuffer[SelectVariable] = ArrayBuffer()
      for (node <- NodeVariable.all) {
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
    operator: String) extends SelectVariable(label) {
    def matched_nodes = {
      operator match {
        case "=" => scope.filter(_.info.attrs(key).toString == value)
        case "<=" => scope.filter(_.info.attrs(key).toString <= value)
        case ">=" => scope.filter(_.info.attrs(key).toString >= value)
      }
    }
    override def toString = s"$label.$key $operator $value"
    override def expression = s"@${key}${operator}'${value}'"
  }

  class TextVariable(
    text: String,
    operator: String) extends SelectVariable(label) {

    def matched_nodes = {
      operator match {
        case "=" => scope.filter(_.info.text == text)
        case ">=" => scope.filter(_.info.text >= text)
        case "<=" => scope.filter(_.info.text <= text)
      }
    }
    override def toString = s"<$label>${operator}$text"
    override def expression = s"text()='$text'"
  }

  abstract class SelectVariable(
    val label: String) extends Variable {
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
