-module(leader_election).
-author("krzemin").
-compile(export_all).

-import(graph, [cycle/1, to_neighbours_array/1]).


%
% This module contains implementation of Chang-Roberts leader election
% in ring algorithm.
%

%% API
%%-export([]).

% init() is entry-point for K-th process waiting to receive a Pid list and
% then immediately invoking p() function which is main algorithm
init(K) ->
  io:format(user, "waiting for neighbours list at process ~p~n", [K]),
  receive
    Neighs ->
      io:format(user, "initializing process ~p with neigbours list ~p~n", [K, Neighs]),
      non_participant(K, Neighs)
  end.


%
% Cases when proces is non participant
% 
% Params:
%   K - process id
%   Neighs - process neighbors
%
% Messages:
%   begin_election - send election message to clockwise neighbor and go to participant state
%
non_participant(K, Neighs) ->
  receive
    begin_election ->
      NextPid = next(K, Neighs),
      NextPid ! {election, self()},
      participant(K, Neighs)
  end.

participant(K, Neighs) ->
  ok.


%
% Utils functions
%

% Get clockwise direction neighbor for K
%
% Params:
%   K - process id
%   Neighs - process neighbors
%
%
next(K, Neighs) ->
  case proplists:get_value(K+1, Neighs) of
    undefined -> proplists:get_value(1, Neighs) ;
    Pid -> Pid
  end.

%
% Get counter-clockwise direction neighbor for K
%
% Params:
%   K - process id
%   Neighs - process neighbors
%
%
prev(1, Neighs) ->
  Keys = proplists:get_keys(Neighs),
  MaxNeighId = lists:max(Keys),
  proplists:get_value(MaxNeighId, Neighs) ;
prev(K, Neighs) -> proplists:get_value(K - 1, Neighs).





setup_local(N) ->
  G = cycle(N),
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


