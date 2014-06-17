-module(graph).
-author("krzemin").

%% API
-export([clique/1, map/2, graph_size/1, to_neighbours_array/1]).

% clique() creates clique of size N represented as set of edges
clique(N) when N < 2 -> [] ;
clique(2) -> [{1,2}] ;
clique(N) ->
  V1 = clique(N - 1),
  Seq = lists:seq(1, N - 1),
  V2 = [{V, N} || V <- Seq],
  V1 ++ V2.

% map() maps a function over a list of edges
map(F, G) ->
  [{F(V), F(U)} || {V, U} <- G].

% graph_size() returns size of graph (N) represented as list of edges
% assuming that verticies are numbered from 1 to N
graph_size([]) -> 0 ;
graph_size([{V1, V2} | Es]) ->
  max(max(V1, V2), graph_size(Es)).


% to_neighbours_array() converts graph represented as list of edges
% to representation which is mapping from i-th node to its direct
% neighbours
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


