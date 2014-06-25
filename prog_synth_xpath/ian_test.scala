package prog_synth_xpath

object ian_test extends App {
	val comments = (DATA.xml \\ "DatabaseAttributes")
	for(comment <- comments) { 
	  println(comment)
	  for(attr <- comment.attributes)
	    println(attr)
	}
}