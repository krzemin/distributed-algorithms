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

%
% This module contains implementation of broadcasting algorithm over
% an arbitrary graph
%


%% API
%%-export([]).

% init() is entry-point for K-th process waiting to receive a Pid list and
% then immediately invoking p() function which is main algorithm
init(K) ->
  receive
    Neighs ->
      io:format(user, "initializing process ~p with neigbours list ~p~n", [K, Neighs]),
      p(K, Neighs)
  end.

% p() is main processing function in 1st state of every process
p(K, Neighs) ->
  receive
    die ->
      io:format(user, "process ~p received die message~n", [K]) ;
    {Parent, M} ->
      io:format(user, "process ~p received message ~p from ~p~n", [K, M, Parent]),
      NeighsWoParent = [{Id, Pid} || {Id, Pid} <- Neighs, Id =/= Parent],
      send_to_neighbours(M, K, NeighsWoParent),
      q(K, Neighs, M, Parent, [Parent]) ;
    M ->
      io:format(user, "process ~p received initial message ~p~n", [K, M]),
      send_to_neighbours(M, K, Neighs),
      q(K, Neighs, M, 0, [])
  end.

% () is main processing function in 2nd stage of every process
q(K, Neighs, M, Parent, Received) ->
  NeighIds = [Id || {Id, _} <- Neighs],
  if
    NeighIds == Received ->
      io:format(user, "process ~p finished broadcasting~n", [K]),
      if
        Parent > 0 ->
          {_, NPid} = proplists:lookup(Parent, Neighs),
          NPid ! {K, M} ;
        true -> finish
      end ;
    true ->
      receive
        {K1, _} ->
          io:format(user, "process ~p received reply from ~p~n", [K, K1]),
          q(K, Neighs, M, Parent, lists:sort([K1|Received]))
      end
  end.

% send_to_neighbours() is helper routine which sends messge M to all neighbours
send_to_neighbours(M, K, Neighs) ->
  lists:foreach(fun({_, Pid}) -> Pid ! {K, M} end, Neighs).

% setup() creates processes collects Pid list and returns it
setup(G) ->
  N = graph_size(G),
  Ids = lists:seq(1, N),
  NArr = to_neighbours_array(G),
  IdsPids = [{K, spawn(fun() -> init(K) end)} || K <- Ids],
  lists:foreach(fun({K, Pid}) ->
      NList = array:get(K, NArr),
      Neighs = [{Id, Pid} || {Id, Pid} <- IdsPids, lists:member(Id, NList)],
      Pid ! Neighs
    end,
    IdsPids),
  IdsPids.




