package prog_synth_xpath

object ian_test extends App {
  DTD.populate
  
  
  for((family, _) <- DTD.all) {
    val elts = (DATA.xml \\ family)
    println(family)
    if(elts.length > 0) {
      println(" attrs:")
      for(attr <- elts(0).attributes) {
        println("  " + attr.key)
      }
      if(elts(0).descendant.length == 1) {
        println(" leaf content:")
    	println("  " + elts(0).text)
      }
      else {
        println(" children:")
        for(child <- elts(0).child) {
          println("  " + child.label)
        }
      }
        
    }
  }
}