-module(cube_broadcast).
-author("krzemin").
-compile(export_all).

%
% This module contains implementation of cube-broadcasting algorithm over
% shortest paths using only specific naming convention (known as Gray code)
% which assigns process a number from 0 to 2^(N-1) and implicitly assumes
% a link between processes which numbers differ at 1 bit (considering binary
% notation)
%

%% API
%%-export([]).

% init() is entry-point for K-th process waiting to receive a Pid list and
% then immediately invoking p() function which is main algorithm
init(K, N) ->
  receive
    Pids ->
      io:format(user, "initializing process ~p with pid list ~p~n", [K, Pids]),
      p(K, N, Pids)
  end.

% p() is main processing function for the algorithm
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

% send_to_neighbours() is helper function which passes a message M to proper
% neighbours
send_to_neighbours(M, K, N, Pids) ->
  Bits = lists:seq(0, N - 1),
  FlippedKs = [flipbit(K, B) || B <- Bits],
  PidsToSend = [ lists:nth(K1 + 1, Pids) || K1 <- FlippedKs],
  lists:foreach(fun(Pid) -> Pid ! {K, M} end, PidsToSend).

% flipbit() flips I-th bit in the number K
flipbit(K, I) ->
  K bxor (1 bsl I).

% diffidx() computes index at which K1 and K2 differ
diffidx(K1, K2) ->
  round(math:log(K1 bxor K2) / math:log(2)).

% setup() creates 2^N processes numbered from 0 to 2^(N-1), sends Pid list
% to every one of them after it is known
setup(N) ->
  Ids = lists:seq(0, round(math:pow(2, N)) - 1),
  Pids = [spawn(fun() -> init(K, N) end) || K <- Ids],
  lists:foreach(fun(Pid) -> Pid ! Pids end, Pids),
  Pids.




