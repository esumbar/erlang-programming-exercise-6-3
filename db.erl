-module (db).
-export ([new/0,write/3,delete/2,read/2,match/2]).

new() ->
	[].

write(Key, Element, Db) ->
	[{Key, Element} | Db].

delete(Key, Db) -> lists:keydelete(Key, 1, Db).

read(Key, Db) ->
	case lists:keyfind(Key, 1, Db) of
		{Key, Element} ->
			{ok, Element};
		false ->
			{error, instance}
	end.

match(_, []) -> [];
match(Element, [{Key, Element} | Tail]) ->
	[Key | match(Element, Tail)];
match(Element, [_ | Tail]) -> match(Element, Tail).
