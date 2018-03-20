-module(tarrys).
-export([main/0, processCode/4]).

-record(node, {name, isInitiator = false, neighbours = [], pid, neighbour_pids = []}).

% Create's each node given a string (equivalent to a line of input)
createNode(Node, Initiator) ->
  List = string:split(Node, " ", all),
  [NodeName | Links] = List,
  IsInitiator = isInitiator(Initiator, NodeName),
  Record = #node{name=NodeName, neighbours=Links, isInitiator=IsInitiator},
  Record.

isInitiator(Initiator, NodeName) -> Initiator == NodeName.

% Given a token and a list of neighbours this function handle the logic of sending the token.
sendToken(OwnPid, Neighbours, Parent, Token, MainPid) ->
  case Neighbours of
    [] -> Parent ! {token, OwnPid, Token, MainPid}, [];
    [Hd | Tl] ->
      if
        Hd =:= Parent -> sendToken(OwnPid, Tl, Parent, Token, MainPid);
        true -> Hd ! {token, OwnPid, Token, MainPid}, Tl
      end
  end.

% Function to be executed inside each process
processCode(Name, Neighbours, Parent, MainPid) ->
  receive
    {neighbours, NeighboursList} ->
      processCode(Name, NeighboursList, Parent, MainPid);
    {token, From, Token, MainPid} ->
      NewToken = Token ++ [Name],
      if
        % if this is the first time recieving a token
        (Parent == mainMethod) and (Neighbours == []) -> MainPid ! NewToken;
        (From == mainMethod) and (Neighbours == []) -> MainPid ! NewToken;
        Parent == null -> processCode(Name, sendToken(self(), Neighbours, From, NewToken, MainPid), From, MainPid);
        true -> processCode(Name, sendToken(self(), Neighbours, Parent, NewToken, MainPid), Parent, MainPid) %clean up
      end;
    _ -> ok
  end.

% Given a node name this function fetches the pid of the node from a list of node records
findNeighbourPid(RecordList, Name) ->
  case RecordList of
    [] -> 0;
    [Hd | Tl] -> if
                    Hd#node.name == Name -> Hd#node.pid;
                    true -> findNeighbourPid(Tl, Name)
                end
  end.

% Read In Input until an empty line is met
getLines(List) ->
  case io:get_line("") of
    "\n" -> List;
    Line  ->
      [Input | _] = string:split(Line, "\n"),
      getLines(List ++ [Input])
  end.

% Main method that spawns nodes and sends the initiator the token to start Tarrys algorithm
main() ->
  Input = getLines([]),

  [Initiator | Nodes] = Input,

  NodeRecords = lists:map(fun(Node) -> createNode(Node, Initiator) end, Nodes),

  % Spawn a process for each node in node records and store the process id along with original records
  NodeRecordsPid = lists:map(fun(NR1) ->
                              Pid = spawn(tarrys, processCode, [NR1#node.name, [], null, self()]),
                              NR1#node{pid=Pid}
                          end, NodeRecords),

  [InitiatorNode] = lists:filter(fun(Node) -> Node#node.isInitiator end, NodeRecordsPid),

  % Populate each node record with the pid's of its neighbour pid's
  NodeRecordsNeighbours = lists:map(fun(Node) ->
                                    Node#node{neighbour_pids=lists:map(fun(Neighbour) ->
                                                                        findNeighbourPid(NodeRecordsPid, Neighbour)
                                                                      end, Node#node.neighbours)
                                            } end, NodeRecordsPid),

  % Send each node process the neighbour pids for which it can send tokens onto
  lists:foreach((fun(Node) ->
                  Node#node.pid ! {neighbours, Node#node.neighbour_pids}
                end), NodeRecordsNeighbours),

  % Send the token to the initiator and begin Tarry's algorithm
  InitiatorNode#node.pid ! {token, mainMethod, [], self()},

  receive
    FinalToken -> utils:print_list_helper(FinalToken)
  end,
  ok.
