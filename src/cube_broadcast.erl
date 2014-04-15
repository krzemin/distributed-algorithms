%%%-------------------------------------------------------------------
%%% @author krzemin
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. kwi 2014 17:27
%%%-------------------------------------------------------------------
-module(cube_broadcast).
-author("krzemin").
-compile(export_all).
%% API
%%-export([]).

init(K, N) ->
  receive
    Pids ->
      io:format(user, "initializing process ~p with pid list ~p~n", [K, Pids]),
      p(K, N, Pids)
  end.

p(K, N, Pids) ->
  receive
    die ->
      io:format(user, "process ~p received die message~n", [K]) ;
    {K1, M} ->
      io:format(user, "process ~p received message ~p from ~p~n", [K, M, K1]),

      p(K, N, Pids) ;
    M ->
      lists:foreach(fun(I) ->
          K1 = flipbit(K, I),
          Pid = lists:nth(K1, Pids),
          Pid ! {K, M}
        end,
        lists:seq(0, N - 1)
      ),
      p(K, N, Pids)
  end.

flipbit(K, I) ->
  K bxor (1 bsl I).

diffidx(K1, K2) ->
  round(math:log(K1 bxor K2) / math:log(2)).


setup(N) ->
  Ids = lists:seq(0, round(math:pow(2, N)) - 1),
  Pids = lists:map(fun(K) -> spawn(fun() -> init(K, N) end) end, Ids),
  lists:foreach(fun(Pid) -> Pid ! Pids end, Pids),
  Pids.




