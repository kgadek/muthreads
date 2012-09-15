-module(ftpc).
-author('Konrad Gądek <kgadek@gmail.com>').

-export([start_bench/3]).
-export([behaviour_info/1]).

-define(HOST, "localhost").

-spec start_bench(module(), pos_integer(),timeout()) -> 
	  {'ok',[pos_integer()]}
	| {'timeout',pos_integer()}
	| {'timeout',pos_integer(),[pos_integer()]}.
%% @doc Runs the test.
%% Spawns N worker threads, each will open FTP session, run benchmark specified
%% in module Mod and then quit.
%%
%% Each Module has to export inets_params/0 and benchmark/2. First should return
%% assoc-list with options for inets:start(ftpc, ...). The latter should run
%% actual benchmark; first parameter is Pid of a FTP session, second is
%% a globally-unique string.
%%
%% If everything goes fine, returns {ok, L}, where L is a list of integers indicating
%% time of running FTP commands from each worker.
%%
%% If timeout occurs when waiting for workers to become ready, returns {timeout, M} where 
%% M is the count of workers that did not report ready.
%%
%% If timeout occurs when waiting for workers to finish test, returns {timeout, M, L} with
%% M and L described as above.
%%
%% Entry condition: inets is already running.
start_bench(Mod,N,T) when N > 0 ->
	clear_msg_queue(),
	L = [start_worker(Mod) || _ <- lists:seq(1,N)],
	Ret = case wait_for_all_workers(N,T) of
		ok ->
			[P ! run || P <- L],
			get_run_summary(N,T);
		{timeout, N} = R ->
			[P ! abort || P <- L],
			R
	end,
	[exit(P,kill) || P <- L],
	Ret.

-spec clear_msg_queue() -> 'ok'.
%% @doc Cleans message queue.
%% Used to cleanup before running test.
clear_msg_queue() ->
	receive _ ->
			clear_msg_queue()
	after 0 ->
			ok end.

-spec wait_for_all_workers(integer(),_) -> 'ok' | {'timeout',integer()}.
%% @doc Waits for all workers reporting ready.
wait_for_all_workers(0,_) -> ok;
wait_for_all_workers(N,T) ->
	receive worker_ready ->
			wait_for_all_workers(N-1,T)
	after T ->
			{timeout, N}
	end.

-spec get_run_summary(integer(),_) -> {'ok',[any()]} | {'timeout',integer(),[any()]}.
get_run_summary(N,T) -> get_run_summary(N,T,[]).

-spec get_run_summary(integer(),_,[any()]) -> {'ok',[any()]} | {'timeout',integer(),[any()]}.
%% @doc Gets results.
get_run_summary(0,_T,Acc) -> {ok, Acc};
get_run_summary(N,T,Acc) ->
	receive {ftpc_done, RT} ->
			get_run_summary(N-1,T,[RT|Acc])
	after T ->
			{timeout, N, Acc}
	end.

-spec start_worker(module()) -> pid().
%% @doc Spawns new worker.
start_worker(Mod) ->
	Parent = self(),
	spawn(fun() -> prepare(Parent, Mod) end).

-spec prepare(pid(), module()) -> 'noop' | {'ftpc_done',integer()}.
%% @doc Worker function: prepare benchmark.
prepare(Parent, Mod) ->
	{N,O,W} = now(),
	Suffix = lists:flatten(io_lib:format("~p.~p.~p", [N,O,W])),
	% here all initialization
	InetsParams = [{host, ?HOST}|Mod:inets_params()],
	Parent ! worker_ready,
	receive
		run ->
			{_N1,O1,W1} = os:timestamp(),
			{ok,Pid} = inets:start(ftpc, InetsParams),
			Mod:benchmark(Pid, Suffix),
			inets:stop(ftpc, Pid),
			{_N2,O2,W2} = os:timestamp(),
			Parent ! {ftpc_done, (O2-O1)*1000000 + (W2-W1)};
		abort -> noop
	end.

-spec behaviour_info(_) -> 'undefined' | [{'benchmark',2} | {'inets_params',0},...].
%% @doc Behaviour specification.
behaviour_info(callbacks) ->
	[{inets_params, 0},
		{benchmark, 2}];
behaviour_info(_) -> undefined.

