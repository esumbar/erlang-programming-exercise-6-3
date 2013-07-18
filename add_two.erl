%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(add_two).
-export([start/0, request/1, loop/0]).

start() ->
  register(add_two, Pid = spawn_link(add_two, loop, [])),
  {ok, Pid}.

request(Int) ->
  add_two ! {request, self(), Int},
  receive
    {result, Result} -> Result
  end.

loop() ->
  receive
    {request, Pid, Msg} ->
       Pid ! {result, Msg + 2},
       loop()
  end.
