package prog_synth_xpath
import scala.io.Source

object DTD extends App {
	val regex = """<!ELEMENT\s+(\w+)\s+""".r
	val dtd = Source.fromFile("./xml/DatabaseInventory.dtd")
//	println(dtd.mkString)
	for(regex(element) <- regex.findAllIn(dtd.mkString)) {
	  println(element)
	}
}