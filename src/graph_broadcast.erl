%%%-------------------------------------------------------------------
%%% @author krzemin
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. kwi 2014 19:34
%%%-------------------------------------------------------------------
-module(graph_broadcast).
-author("krzemin").
-import(graph, [graph_size/1, to_neighbours_array/1]).
-compile(export_all).

%% API
%%-export([]).

init(K) ->
  receive
    Neighs ->
      io:format(user, "initializing process ~p with neigbours list ~p~n", [K, Neighs]),
      p(K, Neighs)
  end.

p(K, Neighs) ->
  receive
    X -> io:format(user, "process ~p received message ~p~n", [K, X])
  end.

setup(G) ->
  N = graph_size(G),
  Ids = lists:seq(1, N),
  NArr = to_neighbours_array(G),
  IdsPids = lists:map(fun(K) -> {K, spawn(fun() -> init(K) end)} end, Ids),
  lists:foreach(fun({K, Pid}) ->
      NList = array:get(K, NArr),
      Neighs = lists:filter(fun({Id, _}) ->
          lists:member(Id, NList)
        end, IdsPids),
      Pid ! Neighs
    end,
    IdsPids),
  IdsPids.




