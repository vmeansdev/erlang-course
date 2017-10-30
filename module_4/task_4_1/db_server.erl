%% task 4.1
-module(db_server).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2, 
         handle_info/2, 
         terminate/2]).

-export([new/1, 
         delete/1,
         delete/2, 
         delete_all_objects/1, 
         insert/3,
         find/2]).

%% Client API

-spec new(Name :: atom()) -> ok | {error, Reason :: term()}.
new(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [],[]).

%% Удаляет таблицу (останавливает процесс)
-spec delete(Name :: atom()) -> ok.
delete(Name) -> 
  gen_server:call(Name, delete).

%% Удаляет запись из таблицы по ключу
-spec delete(Name :: atom(), Key :: term()) -> ok.
delete(Name, Key) -> 
  gen_server:call(Name, {delete, Key}).

%% Удаляет все записи из таблицы
-spec delete_all_objects(Name :: atom()) -> ok.
delete_all_objects(Name) -> 
  gen_server:call(Name, delete_all).

%% Добавляет запись в таблицу или заменяет если ключ уже существует  
-spec insert(Name :: atom(), Key :: term(), Value :: term()) -> ok.
insert(Name, Key, Value) -> 
  gen_server:call(Name, {insert, Key, Value}).

%% Ищет запись по ключу
-spec find(Name :: atom(), Key :: term()) -> {ok, Value :: term()} | {error, not_found}.
find(Name, Key) -> 
  gen_server:call(Name, {find, Key}).

%% Server API

init([]) ->
  Db = db:new(#{batch => 10, type => map}),
  {ok, Db}.

handle_call(delete_all, _From, Db) ->
  NewDb = Db#{data => []},
  {reply, ok, NewDb};

handle_call(delete, _From, Db)->
  {stop, normal, Db};

handle_call({delete, Key}, _From, Db) ->
  NewDb = db:delete(Key, Db),
  {reply, ok , NewDb};

handle_call({insert, Key, Value}, _From, Db) ->
  NewDb = db:write(Key, Value, Db),
  {reply, ok, NewDb};

handle_call({find, Key}, _From, Db) ->
  Result = db:read(Key, Db),
  {reply, Result, Db}.

handle_cast(_Msg, Db) ->
  {noreply, Db}.

handle_info(Msg, Db) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, Db}.

terminate(_Reason, _Db) -> ok.