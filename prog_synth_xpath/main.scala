package prog_synth_xpath

object main extends App{
  DTD.populate
  process_XML.populate
  user_input.parse_user_nodes
  println("user node id ++++++++")
  user_input.get_userNode_id
  process_XML.child_parent.map(s => println(s))
  SelectVariable.populate
  CNF_maker.rule_1
  println("\n\n\n\n\n\n\n")
  for (temp <- SelectVariable.all)
  {
    println(temp)
  }
  CNF_maker.rule_23
  Printer.print_file
  CNF_solver.solve
  
}