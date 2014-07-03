package prog_synth_xpath

import scala.collection.mutable._

class DTD_element(
  val label: String,
  val id: Int,
  val multi: Boolean = false,
  var children_ids: Vector[Int] = Vector()
) {
  def forPrint = {s"label : $label \n id : $id \n multi :  $multi \n child_id : ${children_ids.mkString(", ")}\n"}
}

object DTD_parser {

  var dtd_map: Map[String, DTD_element] = Map()
  var total_id = 0
  def all = dtd_map
  def populate = {
    dtd_map += ("#PCDATA" -> new DTD_element("#PCDATA", total_id))
    val regex = """(<!ELEMENT).*>""".r
    val pattern = """<.*\s+(\w+)\s+""".r
    val dtd = scala.io.Source.fromFile(s"${Data.root}/DatabaseInventory.dtd")
    for (element <- regex.findAllIn(dtd.mkString)) {
      val ele_label: String = element.replaceAll(" +", " ").split(" ")(1)
      val temp_element =
        if (dtd_map.contains((ele_label)))
          dtd_map(ele_label)
        else {
          total_id += 1
          val ele = new DTD_element(ele_label, total_id)
          dtd_map += (ele_label -> ele)
          ele
        }
      val child_id_vector: ArrayBuffer[Int] = ArrayBuffer()
      if (element.indexOf("(") != -1) {
        for (child_label <- element.substring(element.indexOf("(") + 1, element.indexOf(")")).split(" *,( )*")) {
          if (child_label != "#PCDATA"){
	          total_id += 1
	          if (child_label.last == '+'){
	            dtd_map += (child_label.replace("+", "") -> new DTD_element(child_label.replace("+", ""), total_id, true))
	            println(child_label.replace("+", ""))
	          } else
	            dtd_map += (child_label -> new DTD_element(child_label, total_id))
	          child_id_vector += total_id
          }
          else child_id_vector += 0
        }
      }
      temp_element.children_ids = child_id_vector.toVector
    }
    dtd_map.map(s => println(s._1 + " " +s._2.forPrint))
    
  }

}