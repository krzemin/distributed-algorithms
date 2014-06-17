-module(graph_broadcast_test).
-author("krzemin").

-include_lib("eunit/include/eunit.hrl").
-import(graph_broadcast, [setup/1]).
-import(graph, [clique/1,cycle/1]).


spawn_and_die_test() ->
  IdsPids = setup(clique(6)),
  ?assert(6 == length(IdsPids)),
  lists:foreach(fun({_, Pid}) -> Pid ! die end, IdsPids).

broadcast_clique_test() ->
  IdsPids = setup(clique(6)),
  ?assert(6 == length(IdsPids)),
  [{_, P0}|_] = IdsPids,
  P0 ! secret_message.

broadcast_cycle_test() ->
  IdsPids = setup(clique(8)),
  ?assert(8 == length(IdsPids)),
  [{_, P0}|_] = IdsPids,
  P0 ! secret_message.
