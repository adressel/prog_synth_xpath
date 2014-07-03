package prog_synth_xpath
import scala.collection.mutable._
import java.lang.StringBuilder
import scala.collection._
import java.io._
import scala.io._


class CNF(label: Label) {
  def count = clauses.length

  val clauses: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

  def add_input(node_info_ids : Vector[Int]) = {
    //convert node_info id to node_variable id
    val nv_ids = node_info_ids.map(label.NodeVariable.id_map(_))
    for(nv_id <- nv_ids) {
      clauses += nv_id.toString
    }
  }
  
  def create_skeleton = {
    //at least one sv must be true
    val sv_ids = label.SelectVariable.all.map(_.id)
    clauses += sv_ids.mkString(" ")

    for (sv <- label.SelectVariable.all) {
      val matched_nodes = sv.matched_nodes.toSet
      val unmatched_nodes = label.NodeVariable.all.toSet -- matched_nodes

      //sv -> output nodes
      for (node <- matched_nodes)
        clauses += s"-${sv.id} ${node.id}"

      for (node <- unmatched_nodes)
        clauses += s"-${sv.id} -${node.id}"
    }
  }

  def print = {
    val out_file = new File(s"${Data.root}/cnf_files/output.cnf")
    val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out_file)), 8000000)
    val header = s"c output.enc\nc\np cnf ${label.Variable.count} ${clauses.length} \n"
    out.write(header)
    var ruleNum = 0;
    for (clause <- clauses) {
	  out.write(clause)
	  out.write(" 0\n")
	}
    out.flush
    out.close
  }
}








