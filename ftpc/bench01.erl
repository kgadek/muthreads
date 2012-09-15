-module(bench01).
-author('Konrad Gądek <kgadek@gmail.com>').
-behaviour(ftpc).

-export([inets_params/0, benchmark/2]).

inets_params() -> [].

benchmark(Pid, Uniq) ->
	ok = ftp:user(Pid, "anonymous", "anonymous"),
	ok = ftp:cd(Pid, "lol"),
	ok = ftp:send(Pid, "lol", "lol"++Uniq).
