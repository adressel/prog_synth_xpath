package prog_synth_xpath
import scala.xml.MetaData
import scala.collection.mutable.ArrayBuffer

class SelectVariable (
  label: String
) extends Variable 

class AttributeVariable (
  label: String,
  attr: MetaData
) extends SelectVariable(label)

class TextVariable (
  label: String,
  text: String   
) extends SelectVariable(label)

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
