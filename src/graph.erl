-module(graph).
-author("krzemin").

%% API
-export([clique/1]).

clique(N) when N < 2 -> [] ;
clique(2) -> [{1,2}] ;
clique(N) ->
  N1 = clique(N - 1),
  N1.

