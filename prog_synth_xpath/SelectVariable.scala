package prog_synth_xpath
import scala.xml._
import scala.collection.mutable.ArrayBuffer

abstract class SelectVariable (
  val label: String
) extends Variable {
  // matched_nodes keeps track of which nodes are mapped to which
  def scope = (Data.xml \\ label)
  def matched_nodes : NodeSeq
}

class AttributeVariable (
  label: String,
  attr: MetaData,
  operator: String
) extends SelectVariable(label) {
  def matched_nodes = {
    operator match {
      case "==" => scope.filter((x => (x \ s"@${attr.key}").toString == attr.value.text))
      case "<=" => scope.filter((x => (x \ s"@${attr.key}").toString <= attr.value.text))
      case ">=" => scope.filter((x => (x \ s"@${attr.key}").toString >= attr.value.text))
    }
  }
  override def toString = s"$label.${attr.key}${operator}${attr.value}"
}

class TextVariable (
  label: String,
  text: String,
  operator: String
) extends SelectVariable(label) {
  
  def matched_nodes = {
    operator match {
      case "==" => scope.filter((x => x.text == text))
      case ">=" => scope.filter((x => x.text >= text))
      case "<=" => scope.filter((x => x.text <= text))
    }
  }
  
  override def toString = s"<$label> $text"
}

object SelectVariable {
  var svs: ArrayBuffer[SelectVariable] = ArrayBuffer()
  def all = svs

  def add_attribute_variables(label : String, attr: MetaData) {
	svs += new AttributeVariable(label, attr, "==")
	svs += new AttributeVariable(label, attr, "<=")
	svs += new AttributeVariable(label, attr, ">=")
  }
  
  def add_text_variables(label : String, text : String) {
    svs += new TextVariable(label, text, "==")
    svs += new TextVariable(label, text, "<=")
    svs += new TextVariable(label, text, ">=")
  }
  
  def populate = {
    var sv_buffer : ArrayBuffer[SelectVariable] = ArrayBuffer()
    for(node <- Data.xml.descendant) { 
      for(attr <- node.attributes) 
        add_attribute_variables(node.label, attr)
        
      if(node.descendant.length == 1) 
        add_text_variables(node.label, node.text)
    }
  }
}
