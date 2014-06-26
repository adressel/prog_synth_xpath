package prog_synth_xpath

object ian_test extends App {
  SelectVariable.populate
  SelectVariable.all.foreach(println(_))
}