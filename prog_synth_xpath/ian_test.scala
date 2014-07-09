package prog_synth_xpath

import scala.collection.mutable._


object ian_test extends App {
  NodeInfo.populate
  NodeInfo.all.map(s => println(s"${s.id} + ${s.label}"))
  var good_ids = Vector(2)
  var label_name = NodeInfo.all.filter(_.id == good_ids(0))(0).label
  val original_label = label_name
  var bad_ids = NodeInfo.all.filter(_.label == label_name).map(s => s.id).toSet -- good_ids.toSet
  excutr.remaining_bad_ids = bad_ids.toList
  val cvs : ArrayBuffer[String] = ArrayBuffer()
  var num_parents = -1
  while (!excutr.get_isDone){
	  println(excutr.remaining_bad_ids)
	  val temp =  excutr.children_excute(label_name, good_ids, bad_ids.toVector).distinct
	  excutr.forbidden_labels += label_name
	  val cvs_child : ArrayBuffer[String] = ArrayBuffer()
	  val tempset = temp.map(s => (s.label_name,s.text,s.attri))
	  for (cand_sv <- temp){
	    if (tempset.count((cand_sv.label_name, cand_sv.text, cand_sv.attri) == _) == 1){
	      cvs_child += cand_sv.expression
	    } else cvs_child += cand_sv.expression("=")
	  }
	  cvs += cvs_child.distinct.mkString(" and ")
	  
	  label_name = NodeInfo.get_map(NodeInfo.get_map(good_ids(0)).parent_id).label
	  good_ids = good_ids.map(s => NodeInfo.get_map(s).parent_id)
	  bad_ids = excutr.remaining_bad_ids.map(s => NodeInfo.get_map(s).parent_id).toSet
	  excutr.remaining_bad_ids = bad_ids.toList
	  num_parents += 1
  }
  cvs.distinct.map(println(_))

  val xpath = new StringBuilder
  xpath ++= "//" + original_label 
  for (cv <- cvs.distinct){
    xpath ++= "[descendant-or-self::" + cv 
  }
 }
 

//<!-- //GlobalDatabaseName[descendant-or-self::GlobalDatabaseName='production.iDevelopment.info' and parent::*/descendant-or-self::DatabaseName[@Ty='1']] -->

