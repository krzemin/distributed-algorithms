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
      D = diffidx(K1, K),
      send_to_neighbours(M, K, D, Pids),
      p(K, N, Pids) ;
    M ->
      io:format(user, "process ~p received initial message ~p~n", [K, M]),
      send_to_neighbours(M, K, N, Pids),
      p(K, N, Pids)
  end.

send_to_neighbours(M, K, N, Pids) ->
  Bits = lists:seq(0, N - 1),
  FlippedKs = [flipbit(K, B) || B <- Bits],
  PidsToSend = [ lists:nth(K1 + 1, Pids) || K1 <- FlippedKs],
  lists:foreach(fun(Pid) -> Pid ! {K, M} end, PidsToSend).


flipbit(K, I) ->
  K bxor (1 bsl I).

diffidx(K1, K2) ->
  round(math:log(K1 bxor K2) / math:log(2)).


setup(N) ->
  Ids = lists:seq(0, round(math:pow(2, N)) - 1),
  Pids = [spawn(fun() -> init(K, N) end) || K <- Ids],
  lists:foreach(fun(Pid) -> Pid ! Pids end, Pids),
  Pids.




