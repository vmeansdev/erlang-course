-module(db).
-export([new/0,
         new/1,
         proc_loop/1,
         write/3, 
         read/2,
         batch_read/2, 
         match/2, 
         delete/2,
         batch_delete/2, 
         destroy/1]).

-type db()      :: #{}.
-type key()     :: atom().
-type element() :: any().
-type opt()     :: {atom(), non_neg_integer()}.
-type params()  :: [opt()].


proc_loop(Db) ->
  receive
    {From, write, {Key, Element}} ->
      NewDb = write(Key, Element, Db),
      From ! NewDb,
      proc_loop(NewDb);
    {From, read, Key} ->
      NewDb = read(Key, Db),
      From ! NewDb,
      proc_loop(Db);
    {From, batch_read, KeyList} ->
      NewDb = batch_read(KeyList, Db),
      From ! NewDb,
      proc_loop(Db);
    {From, batch_delete, KeyList} ->
      NewDb = batch_delete(KeyList, Db),
      From ! NewDb,
      proc_loop(Db);
    {From, match, Element} ->
      NewDb = match(Element, Db),
      From ! NewDb,
      proc_loop(Db);
    {From, delete, Key} ->
      NewDb = delete(Key, Db),
      From ! NewDb,
      proc_loop(Db)
  end.

-spec new() -> db().
new() -> 
   new({batch, 10}).

-spec new(params()) -> db().
new(Parameters) -> 
  Db = #{data => [], params => Parameters},
  spawn(?MODULE, proc_loop, [Db]).

-spec write(key(), element(), db()) -> db().
write(Key, Element, Pid) when is_pid(Pid) ->  
  case  is_process_alive(Pid) of
    true ->
      Pid ! {self(), write, {Key, Element}},
      receive
        Db -> Db
      end;
    _ ->
      {error, process_is_dead}
  end;

write(Key, Element, Db)  when is_map(Db) ->
    Data = maps:get(data, Db),
    NewData = [{Key, Element} | Data],
    Db#{data := NewData}.


-spec read(key(), db()) -> {ok, element()} | {error, instance}.
read(Key, Pid) when is_pid(Pid) ->
  Pid ! {self(), read, Key},
  receive
    Db -> Db
  end;
read(Key, #{data := [{Key, Val} | _]}) -> {ok, Val};
read(Key, #{data := [_ | T]}=Db)       -> read(Key, Db#{data := T});
read(_,   #{data := []})               -> {error, instance}.

-spec batch_read([key()], db()) -> [{key(), element()}].
batch_read([], Db) -> Db;
batch_read(_,       #{data := []}) -> {error, instance};
batch_read(KeyList, #{params := {batch, Value}}=Db) ->
  KeysLength = length(KeyList),
  if
    KeysLength > Value -> {error, batch_limit};
    true -> batch_read([], KeysLength, KeyList, Db)
  end;
batch_read(KeyList, Pid) when is_pid(Pid) ->
  Pid ! {self(), batch_read, KeyList},
  receive
    Db -> Db
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


-spec batch_delete([key()], db()) -> db().
batch_delete([], Db) -> Db;
batch_delete(_, #{data := []}) -> {error, instance};
batch_delete(KeyList, #{params := {batch, Value}}=Db) ->
  KeysLength = length(KeyList),
  if
    KeysLength > Value -> {error, batch_limit};
    true -> batch_delete(KeysLength, KeyList, Db)
  end;
batch_delete(KeyList, Pid) when is_pid(Pid) ->
  Pid ! {self(), batch_delete, KeyList},
  receive
    Db -> Db
  end.

%% batch_delete helper
batch_delete(Value, [KH | KT], Db) ->
  NewDB = delete(KH, Db),
  batch_delete(Value - 1, KT, NewDB);
batch_delete(0, [], Db) -> Db.

%% match helper
match(Element, Matches, [{Key, Element} | T]) ->
    match(Element, [Key | Matches], T);
match(Element, Matches, [_ | T]) -> match(Element, Matches, T);
match(_Element, Mathces, []) -> Mathces.

-spec match(element(), db()) -> [].
match(Element, #{data := [{Key, Element} | T]}) -> match(Element, [Key], T);
match(Element, #{data := [_ | T]})              -> match(Element, [], T);
match(Element, Pid) when is_pid(Pid) ->
  Pid ! {self(), match, Element},
  receive
    Db -> Db
  end.


%% delete helper
delete(Key, Acc, #{data := [{Key, _} | [{NFKey, NFVal} | T]]}=Db) ->
  delete(Key, [{NFKey, NFVal} | Acc], Db#{data := T});
delete(Key, Acc, #{data := [{Key, _} | T]}=Db) ->
  delete(Key, Acc, Db#{data := T});
delete(Key, Acc, #{data := [{NFKey, NFVal} | T]}=Db) ->
  delete(Key, [{NFKey, NFVal} | Acc], Db#{data := T});
delete(_,   Acc, #{data := []}=Db) ->
  NewDB = Db#{data := Acc},
  NewDB.

%% delete
-spec delete(key(), db()) -> db().
delete(_Key, #{data := []}=Db)    -> Db;
delete(Key, Db)  when is_map(Db)  -> delete(Key, [], Db);
delete(Key, Pid) when is_pid(Pid) ->
  case is_process_alive(Pid) of
    true ->
      Pid ! {self(), delete, Key},
      receive
        Db -> Db
      end;
    _ -> {error, process_is_dead}
  end.


-spec destroy(db()) -> ok.
destroy(Pid) ->
  case is_process_alive(Pid) of
    true ->
      exit(Pid, exit),
      ok;
    _ ->
      {error, process_is_dead}
  end.
