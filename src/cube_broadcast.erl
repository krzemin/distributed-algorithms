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

init(K) ->
  receive
    Pids ->
      io:format(user, "initializing process ~p with pid list ~p~n", [K, Pids]),
      p(K, Pids)
  end.

p(K, Pids) ->
  receive
    die ->
      io:format(user, "process ~p received die message~n", [K])
  end.




setup(N) ->
  Ids = lists:seq(0, round(math:pow(2, N)) - 1),
  Pids = lists:map(fun(K) -> spawn(fun() -> init(K) end) end, Ids),
  lists:foreach(fun(Pid) -> Pid ! Pids end, Pids),
  Pids.




