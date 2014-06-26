package prog_synth_xpath

object main extends App{
  DTD.populate
  process_XML.populate
  user_input.prase_user_nodes
  println("user node id ++++++++")
  user_input.get_userNode_id
  process_XML.child_parent.map(s => println(s))
  CNF_maker.rule_1
//  CNF_maker.clause_buffer_1.map(s => println(s))
//  process_XML.all.map(s => println(s + "\n ================\n"))
}