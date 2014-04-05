-module(graph_test).
-author("krzemin").

-include_lib("eunit/include/eunit.hrl").
-import(graph, [clique/1]).

clique_basic_test() ->
  ?assert(clique(0) == []),
  ?assert(clique(1) == []),
  ?assert(clique(2) == [{1,2}]).

clique_complex_test() ->
  C3 = [{1,2},{1,3},{2,3}],
  ?assert(clique(3) == C3),
  C4 = lists:append(C3, [{1,4},{2,4},{3,4}]),
  ?assert(clique(4) == C4),
  C5 = lists:append(C4, [{1,5},{2,5},{3,5},{4,5}]),
  ?assert(clique(5) == C5),
  C6 = lists:append(C5, [{1,6},{2,6},{3,6},{4,6},{5,6}]),
  ?assert(clique(6) == C6).


