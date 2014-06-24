-module(leader_election_test).
-author("krzemin").

-include_lib("eunit/include/eunit.hrl").

-import(leader_election, [next/2, prev/2]).

next_test() ->
  Neighs = [{1,a}, {2,b}, {3,c}],
  ?assert(next(1, Neighs) == b),
  ?assert(next(2, Neighs) == c),
  ?assert(next(3, Neighs) == a).

prev_test() ->
  Neighs = [{1,a}, {2,b}, {3,c}],
  ?assert(prev(1, Neighs) == c),
  ?assert(prev(2, Neighs) == a),
  ?assert(prev(3, Neighs) == b).
