-module (my_db).
-export ([start/0, stop/0]).
-export ([write/2, delete/1, read/1, match/1]).
-export ([init/0]).

start() ->
	register(my_db, Pid = spawn_link(my_db, init, [])),
	{ok, Pid}.

init() ->
	NewDb = db:new(),
	loop(NewDb).


%% Client functions

stop() -> call(stop).

write(Key, Element) -> call({write, Key, Element}).

delete(Key) -> call({delete, Key}).

read(Key) -> call({read, Key}).

match(Element) -> call({match, Element}).

call(Message) ->
	my_db ! {request, self(), Message},
	receive
		{reply, Reply} ->
			Reply
	end.

%% Server main loop

loop(Db) ->
	receive
		{request, Pid, {write, Key, Element}} ->
			UpdatedDb = db:write(Key, Element, Db),
			reply(Pid, ok),
			loop(UpdatedDb);
		{request, Pid, {delete, Key}} ->
			UpdatedDb = db:delete(Key, Db),
			reply(Pid, ok),
			loop(UpdatedDb);
		{request, Pid, {read, Key}} ->
			Reply = db:read(Key, Db),
			reply(Pid, Reply),
			loop(Db);
		{request, Pid, {match, Element}} ->
			Reply = db:match(Element, Db),
			reply(Pid, Reply),
			loop(Db);
		{request, Pid, stop} ->
			reply(Pid, ok)
	end.

reply(Pid, Reply) ->
	Pid ! {reply, Reply}.
