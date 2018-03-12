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
    [Hd | []] -> io:fwrite(" ~p ", [Hd]);
    [Hd | Tl] -> io:fwrite(" ~p,", [Hd]), print_list_helper(Tl)
  end.
