package prog_synth_xpath

import scala.sys.process._

object CNF_solver {
	var clauses : Array[Int] = Array()
	var runtime = 0.0
	
	def solve = {
		val result = s"${Data.root}zchaff ${Data.root}/cnf_files/output.cnf" !!
		val Some(clause_list) = "(.*)Random Seed Used".r.findFirstMatchIn(result)
		clauses = clause_list.group(1).split(" ").filter(x => x.length > 0 && x(0) != '-').map(x => x.toInt)
		println(result)
		if(clauses.size == 0)
		  println("No solution found!  Printing WHERE clauses:")
		for (id <- clauses){
		  for ((_,input) <- process_XML.xml_map){
		    val tempList = input.filter(_.id == id)
		    if(!tempList.isEmpty) println("hi")
		  }
		}
			
		for (id <- clauses){
		    val tempList = SelectVariable.all.filter(_.id == id)
		    if (!tempList.isEmpty)
		    	println(tempList.toList.mkString(" ") + s" \n")
		}
		val Some(runtime_match) = """Total Run Time\s*(\d+.?\d*)""".r.findFirstMatchIn(result)
//		runtime = runtime_match.group(1).toDouble
//		println(runtime)
	}
}