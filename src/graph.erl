-module(graph).
-author("krzemin").

%% API
-export([clique/1]).

clique(N) when N < 2 -> [] ;
clique(2) -> [{1,2}] ;
clique(N) ->
  V1 = clique(N - 1),
  Sq = lists:seq(1, N - 1),
  V2 = lists:map(fun(V) -> {V, N} end, Sq),
  lists:append(V1, V2).