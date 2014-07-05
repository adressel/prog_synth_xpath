package prog_synth_xpath

object ian_test extends App {
  NodeInfo.populate
  val comments = new Label("Comments", Vector(95), Vector(11,22,34,45,57,68,82,111,126))
  println(comments.Variable.count)
//  println(comments.NodeVariable.id_map)
  val comments_cnf = new CNF(comments, Vector(95), Vector(11,22,34,45,57,68,82,111,126))
//  println(comments_cnf.clauses)
//  comments_cnf.clauses.map(s => println(s))
  comments_cnf.solve
}