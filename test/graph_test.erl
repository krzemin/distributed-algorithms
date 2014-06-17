-module(graph_test).
-author("krzemin").

-include_lib("eunit/include/eunit.hrl").
-import(graph, [clique/1, cycle/1, map/2, graph_size/1, to_neighbours_array/1]).

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

cycle_test() ->
  ?assert(cycle(2) == [{1,2}]),
  ?assert(cycle(3) == [{1,3},{1,2},{2,3}]),
  ?assert(cycle(4) == [{1,4},{1,2},{2,3},{3,4}]),
  ?assert(cycle(5) == [{1,5},{1,2},{2,3},{3,4},{4,5}]).

map_test() ->
  C3 = clique(3),
  C3Plus10 = map(fun(V) -> V + 10 end, C3),
  ?assert(C3Plus10 == [{11,12},{11,13},{12,13}]).

graph_size_test() ->
  ?assert(2 == graph_size(clique(2))),
  ?assert(5 == graph_size(clique(5))),
  ?assert(10 == graph_size(clique(10))),
  ?assert(2 == graph_size([{1,2}])),
  ?assert(3 == graph_size([{1,2}, {2,3}])),
  ?assert(6 == graph_size([{1,2}, {4,3}, {6,1}, {2, 5}])).


to_neighbours_array_test() ->
  NA1 = to_neighbours_array(clique(5)),
  ?assert([1,3,4,5] == array:get(2, NA1)),
  NA2 = to_neighbours_array(clique(10)),
  ?assert([1,2,3,4,5,6,7,8,10] == array:get(9, NA2)),
  NA3 = to_neighbours_array([{1,2},{2,3},{3,4}]),
  ?assert([1,3] == array:get(2, NA3)),
  NA4 = to_neighbours_array([{1,4},{3,8},{6,20}]),
  ?assert([] == array:get(17, NA4)).
