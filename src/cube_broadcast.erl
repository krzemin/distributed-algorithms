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

p(K) ->
  receive
    X ->
      io:format(user, "process ~p received message ~p~n", [K, X])
  end.

setup(N) ->
  Ids = lists:seq(0, round(math:pow(2, N)) - 1),
  lists:map(fun(K) -> spawn(fun() -> p(K) end) end, Ids).



