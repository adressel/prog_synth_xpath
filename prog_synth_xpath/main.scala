package prog_synth_xpath

object main extends App{
  DTD.populate
  process_XML.populate
  user_input.prase_user_nodes
  println("user node id ++++++++")
  user_input.get_userNode_id
}