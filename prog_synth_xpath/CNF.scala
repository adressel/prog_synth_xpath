package prog_synth_xpath
import scala.collection.mutable._
import java.lang.StringBuilder
import scala.collection._
import java.io._
import scala.io._
import scala.sys.process._

class CNF(label: Label, node_info_ids: Vector[Int]) {
  val clauses: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

  create
  print

  def create = {
    //convert node_info id to node_variable id
    val nv_ids = node_info_ids.map(label.NodeVariable.id_map(_))
    for (nv_id <- nv_ids) {
      clauses += nv_id.toString
    }
    
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

  def solve = {
    val result = s"${Data.root}zchaff ${Data.root}/cnf_files/output.cnf" !!
    val Some(clause_list) = "(.*)Random Seed Used".r.findFirstMatchIn(result)
    val clauses = clause_list.group(1).split(" ")
      .filter(x => x.length > 0 && x(0) != '-').map(x => x.toInt)
    val variables = clauses.map(label.Variable.id_map(_))
    val node_variables = variables.collect { case x: label.NodeVariable => x }
    val select_variables = variables.collect { case x: label.SelectVariable => x }

    println(node_variables.length)
    println(node_info_ids.length)
    node_variables.length == node_info_ids.length
  }
}








