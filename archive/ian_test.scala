package prog_synth_xpath

object ian_test extends App {
  SelectVariable.populate
  val jeffery = (Data.xml \\ "Administrator").filter((x => (x \ s"@Extension").toString == "6007"))
  println(jeffery.length)
  val attrs = jeffery(0).attributes
  val ext = attrs.value.text
  println(ext)
}