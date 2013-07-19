%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([start_child/4, stop_child/2]).
-export([print_childlist/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
  register(Name, spawn_link(my_supervisor, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
  process_flag(trap_exit, true),
  loop(start_children(_Id = 1, ChildSpecList)).

start_children(_, []) -> [];
start_children(Id, [{M, F, A} | ChildSpecList]) ->
  case (catch apply(M,F,A)) of
    {ok, Pid} ->
      [{Id, Pid, {M,F,A}}|start_children(Id + 1, ChildSpecList)];
    _ ->
      start_children(Id + 1, ChildSpecList)
  end.

%% The loop of the supervisor waits in a receive clause for EXIT and stop messages. 
%% If a child terminates, the supervisor receives the EXIT signal and restarts the terminated 
%% child, replacing its entry in the list of children stored in the ChildList variable:

restart_child(Pid, ChildList) ->
  {Id, Pid, {M,F,A}} = lists:keyfind(Pid, 2, ChildList),
  {ok, NewPid} = apply(M,F,A),
  [{Id, NewPid, {M,F,A}}|lists:keydelete(Pid,2,ChildList)].

loop(ChildList) ->
  receive
    {'EXIT', Pid, _Reason} ->
      NewChildList = restart_child(Pid, ChildList),
      loop(NewChildList);
    {start_child, From, Child} ->
      {Reply, NewChildList} = start_child(Child, ChildList),
      From ! {reply, Reply},
      loop(NewChildList);
    {stop_child, From, Id} ->
      NewChildList = terminate_child(Id, ChildList),
      From ! {reply, ok},
      loop(NewChildList);
    {print_childlist, From} ->
      From ! {reply, ChildList},
      loop(ChildList);
    {stop, From}  ->
      From ! {reply, terminate(ChildList)}
  end.

%% We stop the supervisor by calling the synchronous client function stop/0. Upon receiving the 
%% stop message, the supervisor runs through the ChildList, terminating the children one by one.
%% Having terminated all the children, the atom ok is returned to the process that initiated 
%% the stop call:

stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end.

terminate([{_, Pid, _} | ChildList]) ->
  exit(Pid, kill),
  terminate(ChildList);
terminate(_ChildList) -> ok.

%% Client and supervisor routines to start a child process after the
%% supervisor has started. A corresponding clause is added to the
%% supervisor loop above.

start_child(Name, Module, Function, Argument) ->
  Name ! {start_child, self(), {Module, Function, Argument}},
  receive {reply, Reply} -> Reply end.

start_child({M, F, A}, ChildList) ->
  Id = next_id(ChildList),
  case (catch apply(M,F,A)) of
    {ok, Pid} ->
      {{ok, Id}, [{Id, Pid, {M,F,A}} | ChildList]};
    _ ->
      {failed, ChildList}
  end.

next_id([]) -> 1;
next_id(ChildList) ->
  {Id, _, _} = hd(lists:reverse(lists:keysort(1, ChildList))),
  Id + 1.

%% Client and supervisor routines for stopping a child.

stop_child(Name, Id) ->
  Name ! {stop_child, self(), Id},
  receive {reply, Reply} -> Reply end.

terminate_child(_, []) -> [];
terminate_child(Id, [{Id, Pid, _} | ChildList]) ->
  unlink(Pid),
  exit(Pid, kill),
  terminate_child(Id, ChildList);
terminate_child(Id, [Child | ChildList]) ->
  [Child | terminate_child(Id, ChildList)].

%% Client routine to print the ChildList.

print_childlist(Name) ->
  Name ! {print_childlist, self()},
  receive {reply, Reply} -> Reply end.
