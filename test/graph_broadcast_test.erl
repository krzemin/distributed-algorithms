-module(graph_broadcast_test).
-author("krzemin").

-include_lib("eunit/include/eunit.hrl").
-import(graph_broadcast, [setup/1]).
-import(graph, [clique/1]).


spawn_and_die_test() ->
  IdsPids = setup(clique(6)),
  ?assert(6 == length(IdsPids)),
  lists:foreach(fun({Id, Pid}) -> Pid ! die end, IdsPids).

