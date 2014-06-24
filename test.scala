package xpath
import scala.xml.XML
object test extends App {
	val xml = XML.loadFile("/Users/Stephen/Desktop/test.xml")
	println(xml)
	println(xml\\"description")
}