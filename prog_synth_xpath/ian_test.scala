package prog_synth_xpath

object ian_test extends App {
  NodeInfo.populate
  val comments = new Label("Comments")
  println(comments.Variable.count)
//  println(comments.NodeVariable.id_map)
  val comments_cnf = new CNF(comments, Vector(95))
  println(comments_cnf.solve)
}