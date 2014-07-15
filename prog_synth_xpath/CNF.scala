package prog_synth_xpath
import scala.collection.mutable._
import java.lang.StringBuilder
import scala.collection._
import java.io._
import scala.io._
import scala.sys.process._
import scala.collection._

class CNF(label: Label, node_info_Goodids: Vector[Int], node_info_Badids: Vector[Int]) {
  val clauses: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

  create
  print

  def create = {
    //convert node_info id to node_variable id
    val nv_Gids = node_info_Goodids.filter(label.NodeVariable.id_map.contains(_)).map(label.NodeVariable.id_map(_))
    for (nv_id <- nv_Gids)
      clauses += nv_id.toString

    val nv_Bids = node_info_Badids.filter(label.NodeVariable.id_map.contains(_)).map(label.NodeVariable.id_map(_))
    for (nv_id <- nv_Bids)
      clauses += "-" + nv_id.toString

    val sv_nv_map: mutable.Map[Int, ArrayBuffer[Int]] = mutable.Map()

    for (sv <- label.SelectVariable.all) {
      val matched_nodes = sv.matched_nodes.toSet
      val unmatched_nodes = label.NodeVariable.all.toSet -- matched_nodes
      clauses += s"${sv.id} " + unmatched_nodes.map(s => s.id).mkString(" ")
      for (node <- unmatched_nodes) {
        clauses += s"-${sv.id} -${node.id}"
        clauses += s"-${sv.id} -${node.output_id}"
        if (!sv_nv_map.contains(node.output_id)) {
          sv_nv_map += (node.output_id -> ArrayBuffer(sv.id))
        } else sv_nv_map(node.output_id) += sv.id
      }
    }
    for (node <- sv_nv_map) {
      clauses += s"${node._1} " + node._2.mkString(" ")
    }
  }

  def print = {
    val out_file = new File(s"${Data.root}/cnf_files/output.cnf")
    val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out_file)), 8000000)
    val header = s"c output.enc\nc\np cnf ${Variable.count} ${clauses.length} \n"
    out.write(header)
    var ruleNum = 0;
    for (clause <- clauses) {
      out.write(clause)
      out.write(" 0\n")
    }
    out.flush
    out.close
  }

  def solve: Tuple2[immutable.Set[NodeVariable], Vector[return_Info]] = {
    val result = s"${Data.root}zchaff ${Data.root}/cnf_files/output.cnf" !!
    val Some(clause_list) = "(.*)Random Seed Used".r.findFirstMatchIn(result)
    val clauses = clause_list.group(1).split(" ")
      .filter(x => x.length > 0 && x(0) != '-').map(x => x.toInt)
    val variables = clauses.map(Variable.id_map(_)).toSet
    val node_variables = variables.collect { case x: NodeVariable => x }
    val select_variables = variables.collect { case x: label.SelectVariable => x }
    (node_variables, select_variables.map(s => s.returnInfo).toVector)
  }
}








