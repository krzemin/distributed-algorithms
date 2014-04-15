-module(cube_broadcast_test).
-author("krzemin").

-include_lib("eunit/include/eunit.hrl").
-import(cube_broadcast, [setup/1]).




spawn_and_die_test() ->
  io:format("hahahaha~n"),
  Pids = cube_broadcast:setup(4),
  ?assert(16 == length(Pids)),
  lists:foreach(fun(Pid) -> Pid ! zdychaj end, Pids).

