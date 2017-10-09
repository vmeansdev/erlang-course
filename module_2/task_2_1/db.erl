-module(db).
-export([new/0,
         new/1,
         write/3, 
         read/2,
         batch_read/2, 
         match/2, 
         delete/2, 
         destroy/1]).

-type db()      :: #{}.
-type key()     :: atom().
-type element() :: any().
-type opt()     :: {atom(), non_neg_integer()}.
-type params()  :: [opt()].

-spec new() -> db().
new() -> #{data => [], params => {batch, 10}}.

-spec new(params()) -> db().
new(Parameters) -> #{data => [], params => Parameters}.

-spec write(key(), element(), db()) -> db().
write(Key, Element, Db) ->
    Data = maps:get(data, Db),
    NewData = [{Key, Element} | Data],
    Db#{data := NewData}.

-spec read(key(), db()) -> {ok, element()} | {error, instance}.
read(Key, #{data := [{Key, Val} | _]}) -> {ok, Val};
read(Key, #{data := [_ | T]}=Db)       -> read(Key, Db#{data := T});
read(_,   #{data := []})               -> {error, instance}.

-spec batch_read([key()], db()) -> [key(), element()].
batch_read([], Db) -> Db;
batch_read(_,       #{data := []}) -> {error, instance};
batch_read(KeyList, #{params := {batch, Value}}=Db) ->
  KeysLength = length(KeyList),
  if
    KeysLength > Value -> {error, batch_limit};
    true -> batch_read([], KeysLength, KeyList, Db)
  end.


%% batch_read helper
batch_read(Acc, Value, [KH | KT], Db) ->
  case read(KH, Db) of
    {ok, Element} ->
      ReadTuple = {KH, Element},
      NewDB = delete(KH, Db),
      batch_read([ReadTuple | Acc], Value - 1, KT, NewDB);
    _ ->
      {error, instance}
  end;
batch_read(Acc, 0, [], _) -> Acc.

%% match helper
match(Element, Matches, [{Key, Element} | T]) ->
    match(Element, [Key | Matches], T);
match(Element, Matches, [_ | T]) -> match(Element, Matches, T);
match(_Element, Mathces, []) -> Mathces.

-spec match(element(), db()) -> [].
match(Element, #{data := [{Key, Element} | T]}) -> match(Element, [Key], T);
match(Element, #{data := [_ | T]})              -> match(Element, [], T).


%% delete helper
delete(Key, Acc, #{data := [{Key, _Val} | [{NFKey, NFVal} | T]]}=Db) ->
    delete(Key, [{NFKey, NFVal} | Acc], Db#{data := T});
delete(Key, Acc, #{data := [{NFKey, NFVal} | T]}=Db) ->
    delete(Key, [{NFKey, NFVal} | Acc], Db#{data := T});
delete(_, Acc, #{data := []}=Db) -> Db#{data := Acc}.

%% delete
-spec delete(key(), db()) -> db().
delete(_Key, #{data := []}) -> [];
delete(Key, Db)             -> delete(Key, [], Db).


-spec destroy(db()) -> ok.
destroy(_Db) -> ok.