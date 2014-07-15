package prog_synth_xpath
import scala.xml._
import scala.collection.mutable.ArrayBuffer
import scala.collection._

class Label2(val label: String) {
  populate

  def populate = {
    Variable.reset
    this.NodeVariable.populate
    this.SelectVariable.populate
  }
  
  def cnf_skeleton = {
    val clauses: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
    val sv_nv_map: mutable.Map[Int, ArrayBuffer[Int]] = mutable.Map()
    for (sv <- this.SelectVariable.all) {
      val matched_nodes = sv.matched_nodes.toSet
      val unmatched_nodes = this.NodeVariable.all.toSet -- matched_nodes
      clauses += s"${sv.id} " + unmatched_nodes.map(s => s.id).mkString(" ")
      for (node <- unmatched_nodes) {
        clauses += s"-${sv.id} -${node.id}"
        clauses += s"-${sv.id} -${node.output_id}"
        if (!sv_nv_map.contains(node.output_id)) {
          sv_nv_map += (node.output_id -> ArrayBuffer(sv.id))
        } else sv_nv_map(node.output_id) += sv.id
      }
    }
    for (node <- sv_nv_map) {
      clauses += s"${node._1} " + node._2.mkString(" ")
    }
    clauses
  }

  object NodeVariable {
    private var nvs: ArrayBuffer[NodeVariable] = ArrayBuffer()
    val id_map: mutable.Map[Int, Int] = mutable.Map()
    def all = nvs
    def populate = {
      //construct NodeVariables from NodeInfos with matching labels
      for (node_info <- NodeInfo.all.filter(_.label == label)) {
        val nv = new NodeVariable(node_info)
        id_map += (node_info.id -> nv.id)
        nvs += nv
      }
    }
  }

  object SelectVariable {
    var svs: ArrayBuffer[SelectVariable] = ArrayBuffer()
    def all = svs

    def add_attribute_variables(attr: MetaData) {
      svs += new AttributeVariable(attr.key, attr.value.text, "<=")
      svs += new AttributeVariable(attr.key, attr.value.text, ">=")
    }

    def add_text_variables(text: String) {
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
        case "<=" => scope.filter(_.info.attrs(key).toString <= value)
        case ">=" => scope.filter(_.info.attrs(key).toString >= value)
      }
    }
    override def toString = s"$label.$key $operator $value"
    override def expression = s"@${key}${operator}'${value}'"
    override def returnInfo = new return_Info(label, operator, value, key)
  }

  class TextVariable(
    text: String,
    operator: String) extends SelectVariable(label) {

    def matched_nodes = {
      operator match {
        case ">=" => scope.filter(_.info.text >= text)
        case "<=" => scope.filter(_.info.text <= text)
      }
    }
    override def toString = s"<$label>${operator}$text"
    override def expression = s"text() $operator '$text'"
    override def returnInfo = new return_Info(label, operator, text)
  }

  abstract class SelectVariable(
    val label: String) extends Variable {
    // matched_nodes keeps track of which nodes are mapped to which
    def scope = NodeVariable.all
    def matched_nodes: ArrayBuffer[NodeVariable]
    def expression: String
    def returnInfo: return_Info
  }
}

