package prog_synth_xpath

import scala.sys.process._
import scala.collection.mutable.ArrayBuffer

//object CNF_solver {
//	var clauses : Array[Int] = Array()
//	var runtime = 0.0
//	
//	val select_variables : ArrayBuffer[SelectVariable] = ArrayBuffer()
//	
//	def solve = {
//		val result = s"${Data.root}zchaff ${Data.root}/cnf_files/output.cnf" !!
//		val Some(clause_list) = "(.*)Random Seed Used".r.findFirstMatchIn(result)
//		clauses = clause_list.group(1).split(" ").filter(x => x.length > 0 && x(0) != '-').map(x => x.toInt)
//		println(result)
//		if(clauses.size == 0)
//		  println("No solution found!  Printing WHERE clauses:")
//			
//		for (id <- clauses)
//		    select_variables ++= SelectVariable.all.filter(_.id == id)
//		
//		val Some(runtime_match) = """Total Run Time\s*(\d+.?\d*)""".r.findFirstMatchIn(result)
////		runtime = runtime_match.group(1).toDouble
////		println(runtime)
//		process
//	}
//	
//	def process = {
//	  //get user input label
//	  val input_label = user_input.xml_user_node_list(0).label
//	  for(sv <- select_variables) {
//        val xpath_expression = s"//$input_label[descendant-or-self::${sv.label}/${sv.expression}]"
//        println(xpath_expression)
//	  }
//	}
//}