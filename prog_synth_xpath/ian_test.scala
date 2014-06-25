package prog_synth_xpath

object ian_test extends App {
  DTD.populate
  
  for((family, _) <- DTD.all) {
    val elts = (DATA.xml \\ family)
    println(family)
  }
//	val comments = (DATA.xml \\ "DatabaseAttributes")
//	for(comment <- comments) { 
////	  println(comment)
//	  for(attr <- comment.attributes)
//	    val (key, value) = attr
//	    println(attr)
//	}
}