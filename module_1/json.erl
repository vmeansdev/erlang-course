%% - task 1.8
-module(json).
-export([new/1, read/2, write/3]).

%% - helper function
new(Map, []) -> Map;
new(Map, [{Key, Value} | T]) ->
  NewMap = maps:put(Key, Value, Map),
  new(NewMap, T).

%% - public function
new([{Key, Value} | T]) ->
  Map = #{Key => Value},
  new(Map, T);
new([]) -> #{};
new(_) -> {error, badarg}.

read(Key, JsonObj) ->
  try maps:get(Key, JsonObj) of
    Value -> {ok, Value}
  catch
    error:{badkey, _} -> {error, not_found};
    error:{badmap, _} -> {error, not_found}
  end.

%% - possible option?
write(Key, Value, JsonObj) ->
  maps:put(Key, Value, JsonObj).
