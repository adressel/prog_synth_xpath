package prog_synth_xpath

import scala.collection.mutable._

class DTD_element(
  val label: String,
  val level: Int,
  val multi: Boolean = false,
  var children_ids: Vector[Int] = Vector(),
  val id : Int = DTD_parser.get_id
) {
  def forPrint = {s"level : $level \nlabel : $label \n id : $id \n multi :  $multi \n child_id : ${children_ids.mkString(", ")}\n"}
}

object DTD_parser {
  var dtd_map: Map[String, DTD_element] = Map()
  var total_id = -1
  def get_id = {total_id += 1; total_id}
  def all = dtd_map
  def populate = {
    dtd_map += ("#PCDATA" -> new DTD_element("#PCDATA", -1))
    val regex = """(<!ELEMENT).*>""".r
    val pattern = """<.*\s+(\w+)\s+""".r
    val dtd = scala.io.Source.fromFile(s"${Data.root}/DatabaseInventory.dtd")
    for (element <- regex.findAllIn(dtd.mkString)) {
      val ele_label: String = element.replaceAll(" +", " ").split(" ")(1)
      val temp_element =
        if (dtd_map.contains((ele_label)))
          dtd_map(ele_label)
        else {
          val ele = new DTD_element(ele_label, 0)
          dtd_map += (ele_label -> ele)
          ele
        }
      val child_level = temp_element.level + 1
      val child_id_vector: ArrayBuffer[Int] = ArrayBuffer()
      if (element.indexOf("(") != -1) {
        for (child_label <- element.substring(element.indexOf("(") + 1, element.indexOf(")")).split(" *,( )*")) {
          if (child_label != "#PCDATA"){
        	  var temp_element : DTD_element = 
	          if (child_label.last == '+')
	            new DTD_element(child_label.replace("+", ""), child_level, true)
	          else
	            new DTD_element(child_label, child_level)
        	  dtd_map += (temp_element.label -> temp_element)
	          child_id_vector += temp_element.id
          }
          else child_id_vector += 0
        }
      }
      temp_element.children_ids = child_id_vector.toVector
    }
    dtd_map.map(s => println(s._1 + " " +s._2.forPrint))
    
  }

}