package prog_synth_xpath
import scala.collection.mutable._
import java.lang.StringBuilder
import scala.collection._

class CNF_maker(label: Label) {
  def count = clauses.length

  val clauses: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

  def create_skeleton = {
    //at least one sv must be true
    val sv_ids = label.SelectVariable.all.map(_.id)
    clauses += sv_ids.mkString(" ")
    
    for(sv <- label.SelectVariable.all) {
      val matched_nodes = sv.matched_nodes.toSet
      val unmatched_nodes = label.NodeVariable.all.toSet -- matched_nodes
      
      //sv -> output nodes
      for (node <- matched_nodes)
        clauses += s"-${sv.id} ${node.id}"

      for (node <- unmatched_nodes)
        clauses += s"-${sv.id} -${node.id}"
    }
  }
}

//object Printer {
//	
//	def print_file = {// almost the same printer as sql
//	    val outputFile = new File(s"${Data.root}/cnf_files/output.cnf")
//	    val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream( outputFile) ), 8000000 )
//		val header = s"c output.enc\nc\np cnf ${Variable.count} ${CNF_maker.count} \n"
//		out.write(header)
//		var ruleNum = 0;
//		for (clause <- CNF_maker.clauses){
//		  ruleNum += 1
//		  out.write(s"c =========  rule $ruleNum  ============\n")
//		  for (rules <- clause){
//		      out.write(rules)
//			  out.write(" 0\n")
//		  } 
//		  out.flush()
//		}
//		out.write(s"c =========  user input  ============\n")
//		NodeVariable.xml_map.map(s => out.write(s._2.mkString("")))
//		out.close()
//	}
//}








