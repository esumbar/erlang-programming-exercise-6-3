%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
	register(mutex, Pid = spawn_link(mutex, init, [])),
	{ok, Pid}.

stop() ->
	mutex ! stop.

wait() ->
	mutex ! {wait, self()},
	receive ok -> ok end.

signal() ->
	mutex ! {signal, self()}, ok.

init() ->
	process_flag(trap_exit, true),
	free().

free() ->
	receive
		{wait, Pid} ->
			try link(Pid) of
				true ->
					Pid ! ok,
					busy(Pid)
			catch
				error:_Error ->
					free()
			end;
		{'EXIT', _Pid, _Reason} ->
			free();
		stop ->
			terminate()
	end.

busy(Pid) ->
	receive
		{signal, Pid} ->
			unlink(Pid),
			free();
		{'EXIT', Pid, _Reason} ->
			free()
	end.

terminate() ->
	receive
		{wait, Pid} ->
			exit(Pid, kill),
			terminate()
	after
		0 -> ok
	end.
