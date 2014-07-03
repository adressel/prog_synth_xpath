package prog_synth_xpath

object ian_test extends App {
  NodeInfo.populate
  val comments = new Label("Comments")
  comments.populate
  val comments_cnf = new CNF(comments)
  comments_cnf.create_skeleton
  println(comments_cnf.clauses.length)
  comments_cnf.clauses.foreach(println)
}