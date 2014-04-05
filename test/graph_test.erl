-module(graph_test).
-author("krzemin").

-include_lib("eunit/include/eunit.hrl").
-import(graph, [clique/1]).

clique_basic_test() ->
  ?assert(clique(0) == []),
  ?assert(clique(1) == []),
  ?assert(clique(2) == [{1,2}]).

clique_complex_test() ->
  ?assert(clique(3) == [{1,2},{1,3},{2,3}]).


