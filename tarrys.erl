-module(tarrys).
-compile(export_all).
-record(node, {name, isInitiator = false, neighbours = [], pid, neighbour_pids = []}).

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

createNode(Node, Initiator) ->
  List = string:split(Node, " ", all),
  [NodeName | Links] = List,
  IsInitiator = isInitiator(Initiator, NodeName),
  Record = #node{name=NodeName, neighbours=Links, isInitiator=IsInitiator},
  Record.

isInitiator(Initiator, NodeName) -> Initiator == NodeName.

processCode(Name) ->
  receive
    token ->
      % Process Operations
      % If Process has no parent -> Populate Parent (Only occurs on the first time this process has been visited)
      % Add proccess name to token string

      % If process has a link which it hasn't forwarded previously send forward token there
      % Else forward token onto parent (where parent is the first process that sent the token to the current process)
      utils:log(Name),
      io:nl(),
      processCode(Name);
    _ -> ok
  end.

main() ->
  File = "input.txt",
  {ok, Bin} = file:read_file(File),
  Input  = string:tokens(binary_to_list(Bin), "\n"),
  [Initiator | Nodes] = Input,
  utils:log_nl("initiator", Initiator),

  print_nodes(Nodes),

  NodeRecords = lists:map(fun(Node) -> createNode(Node, Initiator) end, Nodes),

  NodeRecords2 = lists:map(fun(NR1) ->
                              Pid = spawn(tarrys, processCode, [NR1#node.name]),
                              NR1#node{pid=Pid}
                          end, NodeRecords),

  [Node1 | Rest] = NodeRecords2,
  Result = Node1#node.pid ! token,

  NodeRecords2.
