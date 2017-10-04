%% - task 1.8
-module(json).
-export([new/1, read/2, write/3]).

-type key()        :: string().
-type keySpec()    :: string().
-type basicValue() :: string() | boolean() | integer() | float().
-type valueSpec()  :: basicValue() | [basicValue()] | {key(), valueSpec()} | [{key(), valueSpec()}].
-type jsonObj()    :: map().

%% - 'new' function helper
new(Map, []) -> Map;
new(Map, [{Key, Value} | T]) ->
  NewMap = maps:put(Key, Value, Map),
  new(NewMap, T).

-spec new([{keySpec(), valueSpec()}]) -> jsonObj() | {error, badarg}.
new([{Key, Value} | T]) ->
  Map = #{Key => Value},
  new(Map, T);
new([]) -> #{};
new(_) -> {error, badarg}.

-spec read(keySpec(), jsonObj()) -> {ok, valueSpec()} | {error, not_found}.
read(Key, JsonObj) ->
  try maps:get(Key, JsonObj) of
    Value -> {ok, Value}
  catch
    error:{badkey, _} -> {error, not_found};
    error:{badmap, _} -> {error, not_found}
  end.

%% - possible option?
%% - not sure we can face {error, not_found} case here.
-spec write(keySpec(), valueSpec(), jsonObj()) -> jsonObj().
write(Key, Value, JsonObj) ->
  maps:put(Key, Value, JsonObj).
