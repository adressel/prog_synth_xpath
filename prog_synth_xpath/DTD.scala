package prog_synth_xpath
import scala.io.Source

object DTD{
	var dtd_map:Map[String,Int] = Map()
	var total_id = 0
	 
	def populate ={
		val regex = """<!ELEMENT\s+(\w+)\s+""".r
		val dtd = Source.fromFile("/Users/Stephen/scalaWorkSpace/xpath_T/weka/xml/DatabaseInventory.dtd")
		for(regex(element) <- regex.findAllIn(dtd.mkString)) {
		  total_id += 1
		  dtd_map += (element -> total_id)
		}
//		dtd_map.map(s => println(s))
	}
	
}