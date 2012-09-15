-module(ftpc).
-author('Konrad Gądek <kgadek@gmail.com>').
-compile(export_all).


-define(HOST, "localhost").
-define(USER, "anonymous").
-define(PASS, "anonymous").
-define(FTP_INIT_CD, "lol").

start_bench(N,T) ->
	clear_msg_queue(),
	L = [start_worker() || _ <- lists:seq(1,N)],
	[receive worker_ready -> ok end || _ <- lists:seq(1,N)],
	[P ! run || P <- L],
	get_run_summary(N,T).

clear_msg_queue() ->
	receive _ -> clear_msg_queue()
	after 0 -> ok end.

get_run_summary(N,T) -> get_run_summary(N,T,[]).

get_run_summary(0,_T,Acc) -> {ok, Acc};
get_run_summary(N,T,Acc) ->
	receive
		{ftpc_done, RT} ->
			get_run_summary(N-1,T,[RT|Acc])
	after T ->
			{timeout, -N, Acc}
	end.

start_worker() ->
	spawn(?MODULE, prepare, [self()]).

prepare(Parent) ->
	{N,O,W} = now(),
	Suffix = lists:flatten(io_lib:format("~p.~p.~p", [N,O,W])),
	% here prepare all the stuff that would mess
	Parent ! worker_ready,
	receive
		run ->
			{_N1,O1,W1} = os:timestamp(),
			{ok,Pid} = inets:start(ftpc, [{host, ?HOST}]),
			ftp:user(Pid, ?USER, ?PASS),
			ftp:cd(Pid, ?FTP_INIT_CD),
			ftp:send(Pid, "lol", "lol"++Suffix),
			inets:stop(ftpc, Pid),
			{_N2,O2,W2} = os:timestamp(),
			Parent ! {ftpc_done, (O2-O1)*1000000 + W2-W1};
		abort -> noop
	end.

