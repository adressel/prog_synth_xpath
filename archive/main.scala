package prog_synth_xpath

object main extends App{
  NodeVariable.populate
  user_input.parse_user_nodes
  user_input.get_userNode_id
  SelectVariable.populate
  CNF_maker.create
  Printer.print_file
  CNF_solver.solve
}