package prog_synth_xpath
import scala.io.Source
import scala.collection.mutable.MutableList
import java.io._
import scala.io._
import scala.collection.mutable.ArrayBuffer

object Printer {
	
	def print_file = {// almost the same printer as sql
	    val outputFile = new File(s"${Data.root}/cnf_files/output.cnf")
	    val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream( outputFile) ), 8000000 )
		val header = s"c output.enc\nc\np cnf ${Variable.count} ${CNF_maker.count} \n"
		out.write(header)
		var ruleNum = 0;
		for (clause <- CNF_maker.clauses){
		  ruleNum += 1
		  out.write(s"c =========  rule $ruleNum  ============\n")
		  for (rules <- clause){
		      out.write(rules)
			  out.write(" 0\n")
		  } 
		  out.flush()
		}
		out.write(s"c =========  user input  ============\n")
		process_XML.xml_map.map(s => out.write(s._2.mkString("")))
//		process_XML.xml_map.map(s => print(s._2.mkString("")))
		out.close()
	}
}
