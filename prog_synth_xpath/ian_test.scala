package prog_synth_xpath

object ian_test extends App {
  DTD.populate
  val blah = Data.xml
  for(node <- Data.xml.descendant) { 
    for(attr <- node.attributes) {
      println(attr.key + ":" + attr.value)
    }
    if(node.descendant.length == 1) {
      println(node.text)
    }
  }
}