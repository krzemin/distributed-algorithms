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
      io:format(user, "process ~p started election with candidate ~n", [K, self()]),
      next(K, Neighs) ! {election, self()} ;
    {election, Candidate} ->
      self() ! {election_not_participant, Candidate}
  end,
  participant(K, Neighs).

participant(K, Neighs) ->
  receive
    {election, Candidate} ->
      io:format(user, "process ~p (participant) received election proposal ~p~n", [K, Candidate]),
      if
        self() == Candidate ->
          leader(K, Neighs) ;
        self() < Candidate ->
          io:format(user, "process ~p (participant) is worse candidate; passing received to next process", [K, Candidate]),
          next(K, Neighs) ! {election, Candidate},
          participant(K, Neighs) ;
        self() > Candidate ->
          io:format(user, "process ~p (participant) is better candidate; skipping", [K, Candidate]),
          participant(K, Neighs)
      end ;
    {election_not_participant, Candidate} ->
      io:format(user, "process ~p (not participant) received election proposal ~p~n", [K, Candidate]),
      if
        self() == Candidate ->
          leader(K, Neighs) ;
        self() < Candidate ->
          io:format(user, "process ~p (not participant) is worse candidate; passing received to next process", [K, Candidate]),
          next(K, Neighs) ! {election, Candidate},
          participant(K, Neighs) ;
        self() > Candidate ->
          io:format(user, "process ~p (not participant) is better candidate; passing itself to next process", [K, Candidate]),
          next(K, Neighs) ! {election, self()}
      end
  end.

leader(K, Neighs) ->
  io:format(user, "process ~p has just been elected as a leader", [K]).



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


