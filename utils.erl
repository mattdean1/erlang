-module(utils).
-compile(export_all).


log_nl(Msg, Elem) ->
  io:fwrite("~s: ~s", [Msg, Elem]),
  io:nl().

log(Msg, Elem) ->
  io:fwrite("~s: ~s", [Msg, Elem]).

log(Elem) ->
  io:fwrite("~s", [Elem]).

print_list(List) ->
  io:fwrite("["),
  print_list_helper(List),
  io:fwrite("]").

print_list_helper(List) ->
  case List of
    [] -> ok;
    [Hd | []] -> io:fwrite("~s", [Hd]);
    [Hd | Tl] -> io:fwrite("~s ", [Hd]), print_list_helper(Tl)
  end.

print_node(Node) ->
  List = string:split(Node, " ", all),
  [NodeName | Links] = List,
  utils:log("Node", NodeName),
  utils:log(" "),
  utils:log([" Links: "]),
  utils:print_list(Links),
  io:nl().

print_nodes(Nodes) ->
  lists:foreach(fun(Node) -> print_node(Node), io:nl() end, Nodes).
