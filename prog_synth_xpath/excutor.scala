package prog_synth_xpath

import scala.collection.mutable._

object excutor {
	private var isDone : Boolean = false
	def get_isDone = isDone
	var remaining_bad_ids : List[Int] = List()
	var forbidden_labels : ArrayBuffer[String] = ArrayBuffer()
  
	def children_excute(label_name : String, good_ids : Vector[Int], bad_ids : Vector[Int]) : ArrayBuffer[return_Info] = {
	  val sv : ArrayBuffer[return_Info] = ArrayBuffer() 
		  val comments_cnf = new CNF(new Label(label_name, good_ids, bad_ids), good_ids, bad_ids)
		  val return_selected_node = comments_cnf.solve
		  val return_bad_ids = (return_selected_node._1.map(s => s.info.id) -- good_ids.toSet).toVector
		  if (return_bad_ids.isEmpty) {
		    sv ++= return_selected_node._2
		    isDone = true
		    return sv
		  } else if (return_bad_ids.size < bad_ids.size){
		    if (check_delete_parent(bad_ids.diff(return_bad_ids.toSeq)))
		    	sv ++= return_selected_node._2
		  }

	  	  var new_good_ids : ArrayBuffer[Int] = ArrayBuffer()
	  	  var new_bad_ids : ArrayBuffer[Int] = ArrayBuffer()
		  for (new_g_id <- good_ids) {
		    new_good_ids ++= NodeInfo.all.filter(_.id == new_g_id)(0).children_ids
		  }
	  	  
		  for (new_b_id <- return_bad_ids) {
		    new_bad_ids ++= NodeInfo.all.filter(_.id == new_b_id)(0).children_ids
		  }
		  
	  	  for (child_id <- NodeInfo.all.filter(_.id == good_ids(0))(0).children_ids){
	  	    if(!isDone){
		  	    val new_label_name = NodeInfo.all.filter(_.id == child_id)(0).label
		  	    if (!forbidden_labels.contains(new_label_name))
		  	    sv ++= children_excute(new_label_name, NodeInfo.all.filter(s => new_good_ids.contains(s.id)).filter(_.label == new_label_name).map(s => s.id).toVector
		  	        , NodeInfo.all.filter(s => new_bad_ids.contains(s.id)).filter(_.label == new_label_name).map(s => s.id).toVector)
	  	    } else return sv
	  	  }
	  sv
	}
	
	def check_delete_parent(child_ids : Vector[Int]) : Boolean= {
	  var has_parent = false
	  for (node_id <- child_ids){
	    var nid = node_id
	    while(nid != 0){
	      if (remaining_bad_ids.contains(nid)) has_parent = true
	      remaining_bad_ids = remaining_bad_ids.filter(_ != nid)
	      nid = NodeInfo.get_map(nid).parent_id
	    }
	  }
	  has_parent
	}
}