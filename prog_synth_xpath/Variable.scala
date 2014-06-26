package prog_synth_xpath
import scala.collection.mutable._

object Variable {
	var count = 0;
	def newId() = {count += 1; count}
	
	val all : ArrayBuffer[Variable] = ArrayBuffer()
}

class Variable {
	val id = Variable.newId
	Variable.all += this
}
