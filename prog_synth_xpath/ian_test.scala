package prog_synth_xpath

object ian_test extends App {
  DTD.populate
  val blah = Data.xml \\ "Administrator"
  val jeffrey = (Data.xml \\ "Administrator")
  println(blah.filter(x => (x \ "@Extension").toString == "6007"))
}