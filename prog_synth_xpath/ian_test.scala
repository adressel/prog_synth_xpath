package prog_synth_xpath

import scala.collection.mutable._


object ian_test extends App {
  NodeInfo.populate
  NodeInfo.all.map(s => println(s"${s.id} + ${s.label}"))
  val good_ids = Vector(1,8)
  var label_name = NodeInfo.all.filter(_.id == good_ids(0))(0).label
  var bad_ids = NodeInfo.all.filter(_.label == label_name).map(s => s.id).toSet -- good_ids.toSet 
  val temp =  excutr.children_excute(label_name, good_ids, bad_ids.toVector).distinct
  
//  temp.map(s => println(s.expression))

  println("\n\n\n")
  val cvs_child : ArrayBuffer[String] = ArrayBuffer()
  val tempset = temp.map(s => (s.label_name,s.text,s.attri))
  for (cand_sv <- temp){
    if (tempset.count((cand_sv.label_name, cand_sv.text, cand_sv.attri) == _) == 1){
      cvs_child += cand_sv.expression
    } else cvs_child += cand_sv.expression("=")
  }
  
  cvs_child.distinct.map(s => println(s))
}


