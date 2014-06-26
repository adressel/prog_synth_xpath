package prog_synth_xpath
import scala.xml._
import scala.collection.mutable.ArrayBuffer

abstract class SelectVariable (
  val label: String
) extends Variable {
  def matched_nodes : NodeSeq
}

class AttributeVariable (
  label: String,
  attr: MetaData
) extends SelectVariable(label) {
  def matched_nodes = {
    (Data.xml \\ label).filter((x => (x \ s"@${attr.key}").toString == attr.value))
  }
}

class TextVariable (
  label: String,
  text: String   
) extends SelectVariable(label) {
  def matched_nodes = {
    (Data.xml \\ label).filter(x => x.text == text)
  }
}

object SelectVariable {
  var svs: Vector[SelectVariable] = Vector()
  def all = svs

  def populate = {
    var sv_buffer : ArrayBuffer[SelectVariable] = ArrayBuffer()
    for(node <- Data.xml.descendant) { 
      for(attr <- node.attributes) {
        sv_buffer += new AttributeVariable(node.label, attr)
      }
      if(node.descendant.length == 1) {
        sv_buffer += new TextVariable(node.label, node.text)
      }
    }
  }
}
