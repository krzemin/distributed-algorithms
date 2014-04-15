-module(graph).
-author("krzemin").

%% API
-export([clique/1, map/2, graph_size/1, to_neighbours_array/1]).

-spec clique(N) -> [Edge] when
  N :: non_neg_integer(),
  V :: non_neg_integer(),
  Edge :: {V, V}.

clique(N) when N < 2 -> [] ;
clique(2) -> [{1,2}] ;
clique(N) ->
  V1 = clique(N - 1),
  Sq = lists:seq(1, N - 1),
  V2 = lists:map(fun(V) -> {V, N} end, Sq),
  lists:append(V1, V2).

-spec map(F, [Edge]) -> [Edge] when
  F :: fun((V) -> V),
  V :: non_neg_integer(),
  Edge :: {V, V}.

map(F, G) ->
  lists:map(fun({V,U}) -> {F(V), F(U)} end, G).

graph_size([]) -> 0 ;
graph_size([{V1, V2} | Es]) ->
  max(max(V1, V2), graph_size(Es)).

to_neighbours_array(G) ->
  N = graph_size(G),
  to_neighbours_array(G, array:new(N+1, {default, []})).

to_neighbours_array([], Arr) ->
  array:map(fun(_, L) -> lists:sort(L) end, Arr) ;
to_neighbours_array([{V1, V2}|Es], Arr) ->
  V1List = array:get(V1, Arr),
  V2List = array:get(V2, Arr),
  Arr1 = array:set(V1, [V2|V1List], Arr),
  Arr2 = array:set(V2, [V1|V2List], Arr1),
  to_neighbours_array(Es, Arr2).


