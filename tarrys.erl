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

sendToken(OwnPid, Neighbours, Parent, Token, MainPid) ->
  case Neighbours of
    [] -> Parent ! {token, OwnPid, Token, MainPid}, [];
    [Hd | Tl] ->
      if
        Hd =:= Parent -> sendToken(OwnPid, Tl, Parent, Token, MainPid);
        true -> Hd ! {token, OwnPid, Token, MainPid}, Tl
      end
  end.

processCode(Name, Neighbours, Parent, MainPid) ->
  receive
    {neighbours, NeighboursList} ->
      processCode(Name, NeighboursList, Parent, MainPid);
    %initiate ->
      % send token to a random child
    {token, From, Token, MainPid} ->
      % utils:print_list(Token),
      % io:format("~s", [Name]),
      NewToken = Token ++ [Name],
      % utils:print_list(NewToken),
      if
        % if this is the first time recieving a token
        Parent =:= null -> processCode(Name, sendToken(self(), Neighbours, From, NewToken, MainPid), From, MainPid);
        (Parent =:= mainMethod) and (Neighbours =:= []) -> MainPid ! NewToken;
        true -> processCode(Name, sendToken(self(), Neighbours, Parent, NewToken, MainPid), Parent, MainPid) %clean up
      end;
      % Process Operation
      % If Process has no parent -> Populate Parent (Only occurs on the first time this process has been visited)
      % Add proccess name to token string

      % If process has a link which it hasn't forwarded previously send forward token there
      % Else forward token onto parent (where parent is the first process that sent the token to the current process)
      % utils:log(Name),
      % io:nl(),
      % processCode(Name, Neighbours);
    _ -> ok
  end.

findNeighbourPid(RecordList, Name) ->
  case RecordList of
    [] -> 0;
    [Hd | Tl] -> if
                    Hd#node.name == Name -> Hd#node.pid;
                    true -> findNeighbourPid(Tl, Name)
                end
  end.

main() ->
  File = "input.txt",
  {ok, Bin} = file:read_file(File),
  Input  = string:tokens(binary_to_list(Bin), "\n"),
  [Initiator | Nodes] = Input,
  utils:log_nl("initiator", Initiator),

  print_nodes(Nodes),

  NodeRecords = lists:map(fun(Node) -> createNode(Node, Initiator) end, Nodes),

  NodeRecordsPid = lists:map(fun(NR1) ->
                              Pid = spawn(tarrys, processCode, [NR1#node.name, [], null, self()]),
                              NR1#node{pid=Pid}
                          end, NodeRecords),

  [InitiatorNode] = lists:filter(fun(Node) -> Node#node.isInitiator end, NodeRecordsPid),

  NodeRecordsNeighbours = lists:map(fun(Node) ->
                                    Node#node{neighbour_pids=lists:map(fun(Neighbour) ->
                                                                        findNeighbourPid(NodeRecordsPid, Neighbour)
                                                                      end, Node#node.neighbours)

                                            } end, NodeRecordsPid),

  lists:foreach((fun(Node) ->
                  Node#node.pid ! {neighbours, Node#node.neighbour_pids}
                end), NodeRecordsNeighbours),

  InitiatorNode#node.pid ! {token, mainMethod, [], self()},


  receive
    FinalToken -> utils:print_list(FinalToken)
  end.
