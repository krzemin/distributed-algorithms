-module(leader_election_test).
-author("krzemin").

-include_lib("eunit/include/eunit.hrl").

-import(leader_election, [next/2, setup_local/1]).

next_test() ->
  Neighs = [{1,a}, {2,b}, {3,c}],
  ?assert(next(1, Neighs) == b),
  ?assert(next(2, Neighs) == c),
  ?assert(next(3, Neighs) == a).

leader_election_test() ->
  IdsPids = setup_local(5),
  ?assert(5 == length(IdsPids)),
  [{_,P0}|_] = IdsPids,
  P0 ! begin_election.





