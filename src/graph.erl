-module(graph).
-author("krzemin").

%% API
-export([clique/1, map/2]).

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
