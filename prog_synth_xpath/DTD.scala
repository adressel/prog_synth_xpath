package prog_synth_xpath
import scala.io.Source

object DTD{
	var dtd_map:Map[String,Int] = Map()
	var total_id = 0
	def all = dtd_map
	def populate ={
		val regex = """<!ELEMENT\s+(\w+)\s+""".r
		val dtd = Source.fromFile(s"${Data.root}/DatabaseInventory.dtd")
		for(regex(element) <- regex.findAllIn(dtd.mkString)) {
		  total_id += 1
		  dtd_map += (element -> total_id)
		}
	}
	
}