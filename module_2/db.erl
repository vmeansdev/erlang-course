-module(db).
-export([new/0, write/3, read/2, match/2, delete/2, destroy/1]).

-type db() :: [].
-type key() :: atom().
-type element() :: any().

-spec new() -> db().
new() -> [].

-spec write(key(), element(), db()) -> db().
write(Key, Element, Db) ->
    [{Key, Element} | Db].

-spec read(key(), db()) -> {ok, element()} | {error, instance}.
read(Key, [{Key, Val} | _]) -> {ok, Val};
read(Key, [_ | T])          -> read(Key, T);
read(_Key, [])              -> {error, instance}.


match(Element, Matches, [{Key, Element} | T]) ->
    match(Element, [Key | Matches], T);
match(Element, Matches, [_ | T]) -> match(Element, Matches, T);
match(_Element, Mathces, []) -> Mathces.

-spec match(element(), db()) -> [].
match(Element, [{Key, Element} | T]) -> match(Element, [Key], T);
match(Element, [_ | T]) -> match(Element, [], T).


delete(Key, Acc, [{Key, _Val} | [{NFKey, NFVal} | T]]) ->
    delete(Key, [{NFKey, NFVal} | Acc], T);
delete(Key, Acc, [{NFKey, NFVal} | T]) ->
    delete(Key, [{NFKey, NFVal} | Acc], T);
delete(_, Acc, []) -> Acc.

-spec delete(key(), db()) -> db().
delete(_Key, []) -> [];
delete(Key, Db) -> 
    delete(Key, [], Db).


-spec destroy(db()) -> ok.
destroy(_Db) -> ok.