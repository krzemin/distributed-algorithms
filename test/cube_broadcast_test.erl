-module(cube_broadcast_test).
-author("krzemin").

-include_lib("eunit/include/eunit.hrl").
-import(cube_broadcast, [setup/1, flipbit/2, diffidx/2]).


spawn_and_die_test() ->
  Pids = setup(4),
  ?assert(16 == length(Pids)),
  lists:foreach(fun(Pid) -> Pid ! die end, Pids).

broadcast_test() ->
  Pids = setup(4),
  ?assert(16 == length(Pids)),
  [P0 | _] = Pids,
  P0 ! secret_message.

flipbit_test() ->
  ?assert(1 == flipbit(0, 0)),
  ?assert(0 == flipbit(1, 0)),
  ?assert(3 == flipbit(2, 0)),
  ?assert(2 == flipbit(3, 0)),
  ?assert(0 == flipbit(2, 1)),
  ?assert(2 == flipbit(0, 1)),
  ?assert(6 == flipbit(4, 1)).

diffidx_test() ->
  ?assert(0 == diffidx(1, 0)),
  ?assert(0 == diffidx(3, 2)),
  ?assert(1 == diffidx(3, 1)),
  ?assert(2 == diffidx(4, 0)),
  ?assert(3 == diffidx(10, 2)),
  ?assert(2 == diffidx(15, 11)).


