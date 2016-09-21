%%%-------------------------------------------------------------------
%%% @author Sergey Penkovsky
%%% @copyright (C) 2015, Sergey Penkovsky <sergey.penkovsky@gmail.com>
%%% @doc
%%%    Erlymon is an open source GPS tracking system for various GPS tracking devices.
%%%
%%%    Copyright (C) 2015, Sergey Penkovsky <sergey.penkovsky@gmail.com>.
%%%
%%%    This file is part of Erlymon.
%%%
%%%    Erlymon is free software: you can redistribute it and/or  modify
%%%    it under the terms of the GNU Affero General Public License, version 3,
%%%    as published by the Free Software Foundation.
%%%
%%%    Erlymon is distributed in the hope that it will be useful,
%%%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%    GNU Affero General Public License for more details.
%%%
%%%    You should have received a copy of the GNU Affero General Public License
%%%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%-------------------------------------------------------------------
-module(em_storage).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(gen_server).

-include("em_records.hrl").

%% API
-export([
         start_link/3,
         get_pid/0,

         get_server/0,
         update_server/1,

         create_permission/1,
         delete_permission/1,
         get_permission/2,
         get_permissions_by_user_id/1,
         get_permissions_by_device_id/1,

         create_user/1,
         update_user/1,
         delete_user/1,
         get_user_by_id/1,
         get_user_by_email/1,
         get_users/0,

         create_device/1,
         update_device/1,
         delete_device/1,
         get_device_by_id/1,
         get_device_by_uid/1,
         get_devices/0,

         create_position/1,
         get_last_position/2,
         get_positions/3
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(COLLECTION_SERVERS, <<"servers">>).
-define(COLLECTION_PERMISSIONS, <<"permissions">>).
-define(COLLECTION_USERS, <<"users">>).
-define(COLLECTION_DEVICES, <<"devices">>).
-define(COLLECTION_POSITIONS, <<"positions">>).

-record(state, {topology}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(get_pid() -> pid()).
get_pid() ->
    gen_server:call(?SERVER, {get_pid}).

-spec(get_server() -> {ok, Rec :: #server{}} | {error, string()}).
get_server() ->
    gen_server:call(?SERVER, {get_server}).

-spec(update_server(Rec :: #server{}) -> {ok, #server{}} | {error, string() | [string()]}).
update_server(Rec) ->
    gen_server:call(?SERVER, {update_server, Rec}).

-spec(create_permission(Rec :: #permission{}) -> {ok, #permission{}} | {error, string() | [string()]}).
create_permission(Rec) ->
    gen_server:call(?SERVER, {create_permission, Rec}).

-spec(delete_permission(Rec :: #permission{}) -> {ok, #permission{}} | {error, string() | [string()]}).
delete_permission(Rec) ->
    gen_server:call(?SERVER, {delete_permission, Rec}).

-spec(get_permissions_by_user_id(UserId :: non_neg_integer()) -> {ok, [#permission{}]} | {error, string() | [string()]}).
get_permissions_by_user_id(UserId) ->
    gen_server:call(?SERVER, {get_permissions, #{<<"userId">> => UserId}}).

-spec(get_permissions_by_device_id(DeviceId :: non_neg_integer()) -> {ok, [#permission{}]} | {error, string() | [string()]}).
get_permissions_by_device_id(DeviceId) ->
  gen_server:call(?SERVER, {get_permissions, #{<<"deviceId">> => DeviceId}}).

-spec(get_permission(UserId :: non_neg_integer(), DeviceId :: non_neg_integer()) -> {ok, #permission{}} | {error, string() | [string()]}).
get_permission(UserId, DeviceId) ->
  gen_server:call(?SERVER, {get_permission, #{<<"userId">> => UserId, <<"deviceId">> => DeviceId}}).



-spec(create_user(Rec :: #user{}) -> {ok, #user{}} | {error, string() | [string()]}).
create_user(Rec) ->
    gen_server:call(?SERVER, {create_user, Rec}).

-spec(delete_user(Rec :: #user{}) -> {ok, #user{}} | {error, string() | [string()]}).
delete_user(Rec) ->
    gen_server:call(?SERVER, {delete_user, Rec}).

-spec(update_user(Rec :: #user{}) -> {ok, #user{}} | {error, string() | [string()]}).
update_user(Rec) ->
    gen_server:call(?SERVER, {update_user, Rec}).

-spec(get_user_by_id(Id :: non_neg_integer()) -> {ok, #user{}} | {error, string() | [string()]}).
get_user_by_id(Id) ->
    gen_server:call(?SERVER, {get_user, #{<<"id">> => Id}}).

-spec(get_user_by_email(Email :: string()) -> {ok, #user{}} | {error, string() | [string()]}).
get_user_by_email(Email) ->
    gen_server:call(?SERVER, {get_user, #{<<"email">> => Email}}).

-spec(get_users() -> {ok, [#user{}]} | {error, string() | [string()]}).
get_users() ->
    gen_server:call(?SERVER, {get_users}).



-spec(create_device(Rec :: #device{}) -> {ok, #device{}} | {error, string() | [string()]}).
create_device(Rec) ->
    gen_server:call(?SERVER, {create_device, Rec}).

-spec(delete_device(Rec :: #device{}) -> {ok, #device{}} | {error, string() | [string()]}).
delete_device(Rec) ->
    gen_server:call(?SERVER, {delete_device, Rec}).

-spec(update_device(Rec :: #device{}) -> {ok, #device{}} | {error, string() | [string()]}).
update_device(Rec) ->
    gen_server:call(?SERVER, {update_device, Rec}).

-spec(get_device_by_id(Id :: non_neg_integer()) -> {ok, #device{}} | {error, string() | [string()]}).
get_device_by_id(Id) ->
    gen_server:call(?SERVER, {get_device, #{<<"id">> => Id}}).

-spec(get_device_by_uid(UniqueId :: string()) -> {ok, #device{}} | {error, string() | [string()]}).
get_device_by_uid(UniqueId) ->
    gen_server:call(?SERVER, {get_device, #{<<"uniqueId">> => UniqueId}}).

-spec(get_devices() -> {ok, [#device{}]} | {error, string() | [string()]}).
get_devices() ->
    gen_server:call(?SERVER, {get_devices}).


-spec(create_position(Rec :: #position{}) -> {ok, #position{}} | {error, string() | [string()]}).
create_position(Rec) ->
    gen_server:call(?SERVER, {create_position, Rec}).

-spec(get_last_position(Id :: non_neg_integer(), DeviceId :: non_neg_integer()) -> {ok, #position{}} | {error, string() | [string()]}).
get_last_position(Id, DeviceId) ->
    gen_server:call(?SERVER, {get_position, #{<<"id">> => Id, <<"deviceId">> => DeviceId}}).

-spec(get_positions(DeviceId :: non_neg_integer(), From :: non_neg_integer(), To :: non_neg_integer()) -> {ok, [#position{}]}).
get_positions(DeviceId, From, To) ->
    gen_server:call(?SERVER, {get_positions, #{<<"deviceId">> => DeviceId, <<"fixTime">> => {'$gte', From, '$lte', To}}}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Seed :: term(), Options :: term(), WorkerOptions :: term()) ->
             {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Seed, Options, WorkerOptions) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Seed, Options, WorkerOptions], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
             {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term()} | ignore).
init([Seed, Options, WorkerOptions]) ->
    {ok, Topology} = mongoc:connect(Seed, Options, WorkerOptions),
    State = #state{topology = Topology},
    do_create_indexses(State),
    case do_get_server(State) of
        {error, _Reason} ->
            do_create_server(State, #server{id = gen_uid()});
        {ok, _Server} -> void
    end,
    case do_count_users(State) of
        0 ->
            do_create_user(State, #user{
                                     name = <<"admin">>,
                                     email = <<"admin">>,
                                     password = <<"admin">>,
                                     admin = true
                                    });
        _ -> void
    end,
    em_logger:info("COUNT: ~w", [do_count_users(State)]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
             {reply, Reply :: term(), NewState :: #state{}} |
             {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_call({get_pid}, _From, State) ->
    {reply, State#state.topology, State};

handle_call({get_server}, _From, State) ->
    {reply, do_get_server(State), State};
handle_call({update_server, Rec}, _From, State) ->
    {reply, do_update_server(State, Rec), State};

handle_call({create_permission, Rec}, _From, State) ->
    {reply, do_create_permission(State, Rec), State};
handle_call({delete_permission, Rec}, _From, State) ->
    {reply, do_delete_permission(State, Rec), State};
handle_call({get_permission, Query}, _From, State) ->
    {reply, do_get_permission(State, Query), State};
handle_call({get_permissions, Query}, _From, State) ->
  {reply, do_get_permissions(State, Query), State};

handle_call({create_user, Rec}, _From, State) ->
    {reply, do_create_user(State, Rec), State};
handle_call({update_user, Rec}, _From, State) ->
    {reply, do_update_user(State, Rec), State};
handle_call({delete_user, Rec}, _From, State) ->
    {reply, do_delete_user(State, Rec), State};
handle_call({get_user, Query}, _From, State) ->
    {reply, do_get_user(State, Query), State};
handle_call({get_users}, _From, State) ->
    {reply, do_get_users(State), State};

handle_call({create_device, Rec}, _From, State) ->
    {reply, do_create_device(State, Rec), State};
handle_call({update_device, Rec}, _From, State) ->
    {reply, do_update_device(State, Rec), State};
handle_call({delete_device, Rec}, _From, State) ->
    {reply, do_delete_device(State, Rec), State};
handle_call({get_device, Query}, _From, State) ->
    {reply, do_get_device(State, Query), State};
handle_call({get_devices}, _From, State) ->
    {reply, do_get_devices(State), State};

handle_call({create_position, Rec}, _From, State) ->
    {reply, do_create_position(State, Rec), State};
handle_call({get_position, Query}, _From, State) ->
    {reply, do_get_position(State, Query), State};
handle_call({get_positions, Query}, _From, State) ->
    {reply, do_get_positions(State, Query), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, #state{topology = Topology}) ->
    mongoc:disconnect(Topology),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
             {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_create_indexses(#state{topology = Topology}) ->
    Callback = fun(Worker) ->
                       Indexs = [
                                 {?COLLECTION_SERVERS, #{<<"key">> => #{<<"id">> => 1}, <<"name">> => <<"id_1">>, <<"unique">> => true}},

                                 {?COLLECTION_PERMISSIONS, #{<<"key">> => #{<<"id">> => 1}, <<"name">> => <<"id_1">>, <<"unique">> => true}},
                                 {?COLLECTION_PERMISSIONS, #{<<"key">> => #{<<"userId">> => 1, <<"deviceId">> => 1}, <<"name">> => <<"userId_1_deviceId_1">>, <<"unique">> => true}},

                                 {?COLLECTION_USERS, #{<<"key">> => #{<<"id">> => 1}, <<"name">> => <<"id_1">>, <<"unique">> => true}},
                                 {?COLLECTION_USERS, #{<<"key">> => #{<<"email">> => 1}, <<"name">> => <<"email_1">>, <<"unique">> => true}},

                                 {?COLLECTION_DEVICES, #{<<"key">> => #{<<"id">> => 1}, <<"name">> => <<"id_1">>, <<"unique">> => true}},
                                 {?COLLECTION_DEVICES, #{<<"key">> => #{<<"uniqueId">> => 1}, <<"name">> => <<"uniqueId_1">>, <<"unique">> => true}},

                                 {?COLLECTION_POSITIONS, #{<<"key">> => #{<<"id">> => 1}, <<"name">> => <<"id_1">>, <<"unique">> => true}},
                                 {?COLLECTION_POSITIONS, #{<<"key">> => #{<<"deviceId">> => 1, <<"fixTime">> => 1, <<"imei">> => 1}, <<"name">> => <<"deviceId_1_fixTime_1_imei_1">>, <<"unique">> => true}}
                                ],
                       lists:map(fun({Collection, Index}) -> mc_worker_api:ensure_index(Worker, Collection, Index) end, Indexs)
               end,
    mongoc:transaction(Topology, Callback).

do_get_server(#state{topology = Topology}) ->
    Callback = fun(Conf) ->
                       mongoc:find_one(Conf, ?COLLECTION_SERVERS, #{}, #{}, 0)
               end,
    Item = mongoc:transaction_query(Topology, Callback),
    do_get_result(Item, server).

do_update_server(#state{topology = Topology}, Rec) ->
    Map = to_map(server, Rec),
    Callback = fun(Worker) ->
                       Key = #{<<"id">> => Rec#server.id},
                       Command = #{<<"$set">> => maps:remove(<<"id">>, Map)},
                       mc_worker_api:update(Worker, ?COLLECTION_SERVERS, Key, Command)
               end,
    Res = mongoc:transaction(Topology, Callback),
    do_update_result(Res, Map, server).

do_create_server(#state{topology = Topology}, Rec) ->
    Map = to_map(server, Rec),
    Callback = fun(Worker) ->
                       mc_worker_api:insert(Worker, ?COLLECTION_SERVERS, Map)
               end,
    Item = mongoc:transaction(Topology, Callback),
    do_create_result(Item, server).



do_create_permission(#state{topology = Topology}, Rec) ->
    Map = to_map(permission, Rec#permission{id = gen_uid()}),
    Callback = fun(Worker) ->
                       mc_worker_api:insert(Worker, ?COLLECTION_PERMISSIONS, Map)
               end,
    Item = mongoc:transaction(Topology, Callback),
    do_create_result(Item, permission).

do_delete_permission(#state{topology = Topology}, Rec = #permission{userId = UserId}) when UserId == 0 ->
    Callback = fun(Worker) ->
                       mc_worker_api:delete(Worker, ?COLLECTION_PERMISSIONS, #{<<"deviceId">> => Rec#permission.deviceId})
               end,
    Res = mongoc:transaction(Topology, Callback),
    do_delete_result(Res, to_map(permission, Rec), permission);
do_delete_permission(#state{topology = Topology}, Rec) ->
    Callback = fun(Worker) ->
                       mc_worker_api:delete(Worker, ?COLLECTION_PERMISSIONS, to_map(permission, Rec))
               end,
    Res = mongoc:transaction(Topology, Callback),
    do_delete_result(Res, to_map(permission, Rec), permission).


do_get_permission(#state{topology = Topology}, Query) ->
  Callback = fun(Conf) ->
                mongoc:find_one(Conf, ?COLLECTION_PERMISSIONS, Query, #{}, 0)
             end,
  Item = mongoc:transaction_query(Topology, Callback),
  em_logger:info("RESULT: ~w", [Item]),
  do_get_result(Item, permission).

do_get_permissions(#state{topology = Topology}, Query) ->
    Callback = fun(Conf) ->
                       Cursor = mongoc:find(Conf, ?COLLECTION_PERMISSIONS, Query),
                       Items = mc_cursor:map(fun(Item) -> from_map(permission, Item) end, Cursor, infinity),
                       mc_cursor:close(Cursor),
                       {ok, Items}
               end,
    mongoc:transaction_query(Topology, Callback).

do_count_users(#state{topology = Topology}) ->
    mongoc:transaction(Topology, fun(Worker) -> mc_worker_api:count(Worker, ?COLLECTION_USERS, #{}) end).

do_create_user(#state{topology = Topology}, Rec) ->
    Map = to_map(user, Rec#user{id = gen_uid(), hashPassword = hash(Rec#user.password)}),
    Callback = fun(Worker) ->
                       mc_worker_api:insert(Worker, ?COLLECTION_USERS, Map)
               end,
    Item = mongoc:transaction(Topology, Callback),
    do_create_result(Item, user).


do_update_user(#state{topology = Topology}, Rec) ->
    FixUser = fun(UserModel = #user{password = Password}) ->
                      case Password of
                          undefinded ->
                              UserModel0 = maps:remove(<<"password">>, to_map(user, UserModel)),
                              maps:remove(<<"hashPassword">>, UserModel0);
                          <<"">> ->
                              UserModel0 = maps:remove(<<"password">>, to_map(user, UserModel)),
                              maps:remove(<<"hashPassword">>, UserModel0);
                          _ ->
                              to_map(user, UserModel#user{hashPassword = hash(Password)})
                      end
              end,

    Map = FixUser(Rec),
    Callback = fun(Worker) ->
                       Key = #{<<"id">> => Rec#user.id},
                       Command = #{<<"$set">> => Map},
                       mc_worker_api:update(Worker, ?COLLECTION_USERS, Key, Command)
               end,
    Res = mongoc:transaction(Topology, Callback),
    do_update_result(Res, Map, user).

do_delete_user(#state{topology = Topology}, Rec) ->
    Callback = fun(Worker) ->
                       mc_worker_api:delete(Worker, ?COLLECTION_USERS, #{<<"id">> => Rec#user.id})
               end,
    Res = mongoc:transaction(Topology, Callback),
    do_delete_result(Res, to_map(user, Rec), user).


do_get_users(#state{topology = Topology}) ->
    Callback = fun(Conf) ->
                       Cursor = mongoc:find(Conf, ?COLLECTION_USERS, #{}),
                       Items = mc_cursor:map(fun(Item) -> from_map(user, Item) end, Cursor, infinity),
                       mc_cursor:close(Cursor),
                       {ok, Items}
               end,
    mongoc:transaction_query(Topology, Callback).

do_get_user(#state{topology = Topology}, Query) ->
    Callback = fun(Conf) ->
                       mongoc:find_one(Conf, ?COLLECTION_USERS, Query, #{}, 0)
               end,
    Item = mongoc:transaction_query(Topology, Callback),
    do_get_result(Item, user).



do_create_device(#state{topology = Topology}, Rec) ->
    Map = to_map(device, Rec#device{id = gen_uid(), status = <<"">>, lastUpdate = 0, positionId = 0}),
    Callback = fun(Worker) ->
                       mc_worker_api:insert(Worker, ?COLLECTION_DEVICES, Map)
               end,
    Item = mongoc:transaction(Topology, Callback),
    do_create_result(Item, device).

do_update_device(#state{topology = Topology}, Rec) ->
    Map = to_map(device, Rec),
    Callback = fun(Worker) ->
                       Key = #{<<"id">> => Rec#device.id},
                       Command = #{<<"$set">> => maps:remove(<<"id">>, Map)},
                       mc_worker_api:update(Worker, ?COLLECTION_DEVICES, Key, Command)
               end,
    Res = mongoc:transaction(Topology, Callback),
    do_update_result(Res, Map, device).


do_delete_device(#state{topology = Topology}, Rec) ->
    Callback = fun(Worker) ->
                       mc_worker_api:delete(Worker, ?COLLECTION_DEVICES, #{<<"id">> => Rec#device.id})
               end,
    Res = mongoc:transaction(Topology, Callback),
    do_delete_result(Res, to_map(device, Rec), device).


do_get_devices(#state{topology = Topology}) ->
    Callback = fun(Conf) ->
                       Cursor = mongoc:find(Conf, ?COLLECTION_DEVICES, #{}),
                       Items = mc_cursor:map(fun(Item) -> from_map(device, Item) end, Cursor, infinity),
                       mc_cursor:close(Cursor),
                       {ok, Items}
               end,
    mongoc:transaction_query(Topology, Callback).

do_get_device(#state{topology = Topology}, Query) ->
    Callback = fun(Conf) ->
                       mongoc:find_one(Conf, ?COLLECTION_DEVICES, Query, #{}, 0)
               end,
    Item = mongoc:transaction_query(Topology, Callback),
    do_get_result(Item, device).



do_create_position(#state{topology = Topology}, Rec) ->
    ServerTime = em_helper_time:timestamp(),
    DeviceTime = Rec#position.deviceTime,
    Map = to_map(position, Rec#position{
                             id = gen_uid(),
                             type = <<>>,
                             serverTime = ServerTime,
                             fixTime = fix_time(ServerTime, DeviceTime)
                            }),
    Callback = fun(Worker) ->
                       mc_worker_api:insert(Worker, ?COLLECTION_POSITIONS, Map)
               end,
    Item = mongoc:transaction(Topology, Callback),
    do_create_result(Item, position).

do_get_position(#state{topology = Topology}, Query) ->
    Callback = fun(Conf) ->
                       mongoc:find_one(Conf, ?COLLECTION_POSITIONS, Query, #{}, 0)
               end,
    Item = mongoc:transaction_query(Topology, Callback),
    do_get_result(Item, position).

do_get_positions(#state{topology = Topology}, Query) ->
    Callback = fun(Conf) ->
                       Cursor = mongoc:find(Conf, ?COLLECTION_POSITIONS, Query),
                       Items = mc_cursor:map(fun(Item) -> from_map(position, Item) end, Cursor, infinity),
                       mc_cursor:close(Cursor),
                       {ok, Items}
               end,
    mongoc:transaction_query(Topology, Callback).

do_get_result(Item, ItemType) ->
    do_get_result(maps:is_key(<<"id">>, Item), Item, ItemType).

do_get_result(true, Item, ItemType) ->
    {ok, from_map(ItemType, Item)};
do_get_result(false, _Item, _ItemType) ->
    {error, <<"Not find item">>}.

do_create_result({{true, #{<<"n">> := 1}}, Item}, ItemType) ->
    {ok, from_map(ItemType, Item)};
do_create_result({{true, #{<<"n">> := 0, <<"writeErrors">> := WriteErrors}}, _}, _ItemType) ->
    {error, lists:map(fun(#{<<"errmsg">> := ErrMsg}) -> ErrMsg end, WriteErrors)}.

do_update_result({true, #{<<"n">> := 1, <<"nModified">> := 0}}, _Item, _ItemType) ->
  {warning, <<"Not update item">>};
do_update_result({true, #{<<"n">> := 1, <<"nModified">> := 1}}, Item, ItemType) ->
  {ok, from_map(ItemType, Item)};
do_update_result({true, #{<<"n">> := 0, <<"nModified">> := 0, <<"writeErrors">> := WriteErrors}}, _Item, _ItemType) ->
    {error, lists:map(fun(#{<<"errmsg">> := ErrMsg}) -> ErrMsg end, WriteErrors)};
do_update_result({true, #{<<"n">> := 0, <<"nModified">> := 0}}, _Item, _ItemType) ->
  {warning, <<"Not update item">>}.

do_delete_result({true, #{<<"n">> := 1}}, Item, ItemType) ->
    {ok, from_map(ItemType, Item)};
do_delete_result({true, #{<<"n">> := 0}}, _Item, _ItemType) ->
    {error, <<"Not delete item">>}.

fix_time(ServerTime, DeviceTime) when ServerTime < DeviceTime ->
    ServerTime;
fix_time(_, DeviceTime) ->
    DeviceTime.

from_map(server, Map) ->
    #server{
       id = maps:get(<<"id">>, Map, 0),
       registration = maps:get(<<"registration">>, Map, true),
       readonly = maps:get(<<"readonly">>, Map, false),
       map = maps:get(<<"map">>, Map, <<"">>),
       bingKey = maps:get(<<"bingKey">>, Map, <<"">>),
       mapUrl = maps:get(<<"mapUrl">>, Map, <<"">>),
       language = maps:get(<<"language">>, Map, <<"">>),
       distanceUnit = maps:get(<<"distanceUnit">>, Map, <<"">>),
       speedUnit = maps:get(<<"speedUnit">>, Map, <<"">>),
       latitude = maps:get(<<"latitude">>, Map, 0),
       longitude = maps:get(<<"longitude">>, Map, 0),
       zoom = maps:get(<<"zoom">>, Map, 0)
      };
from_map(permission, Map) ->
    #permission{
       id = maps:get(<<"id">>, Map, 0),
       userId = maps:get(<<"userId">>, Map, 0),
       deviceId = maps:get(<<"deviceId">>, Map, 0)
      };
from_map(user, Map) ->
    #user{
       id = maps:get(<<"id">>, Map, 0),
       name = maps:get(<<"name">>, Map, <<"">>),
       email = maps:get(<<"email">>, Map, <<"">>),
       readonly = maps:get(<<"readonly">>, Map, false),
       admin = maps:get(<<"admin">>, Map, false),
       map = maps:get(<<"map">>, Map, <<"">>),
       language = maps:get(<<"language">>, Map, <<"">>),
       distanceUnit = maps:get(<<"distanceUnit">>, Map, <<"">>),
       speedUnit = maps:get(<<"speedUnit">>, Map, <<"">>),
       latitude = maps:get(<<"latitude">>, Map, 0),
       longitude = maps:get(<<"longitude">>, Map, 0),
       zoom = maps:get(<<"zoom">>, Map, 0),
       password = maps:get(<<"password">>, Map, <<"">>),
       hashPassword = maps:get(<<"hashPassword">>, Map, <<"">>),
       salt = maps:get(<<"salt">>, Map, <<"">>)
      };
from_map(device, Map) ->
    #device{
       id = maps:get(<<"id">>, Map, 0),
       name = maps:get(<<"name">>, Map, <<"">>),
       uniqueId = maps:get(<<"uniqueId">>, Map, <<"">>),
       status = maps:get(<<"status">>, Map, <<"">>),
       lastUpdate = maps:get(<<"lastUpdate">>, Map, 0),
       positionId = maps:get(<<"positionId">>, Map, 0)
      };
from_map(position, Map) ->
    #position{
       id = maps:get(<<"id">>, Map, 0),
       type = maps:get(<<"type">>, Map, <<"">>),
       protocol = maps:get(<<"protocol">>, Map, <<"">>),
       serverTime = maps:get(<<"serverTime">>, Map, 0),
       deviceTime = maps:get(<<"deviceTime">>, Map, 0),
       fixTime = maps:get(<<"fixTime">>, Map, 0),
       deviceId = maps:get(<<"deviceId">>, Map, 0),
       outdated = maps:get(<<"outdated">>, Map, false),
       valid = maps:get(<<"valid">>, Map, false),
       latitude = maps:get(<<"latitude">>, Map, 0.0),
       longitude = maps:get(<<"longitude">>, Map, 0.0),
       altitude = maps:get(<<"altitude">>, Map, 0.0),
       speed = maps:get(<<"speed">>, Map, 0.0),
       course = maps:get(<<"course">>, Map, 0.0),
       address = maps:get(<<"address">>, Map, <<"">>),
       attributes = maps:get(<<"attributes">>, Map, #{})
      }.

to_map(server, Rec) ->
    #{
       <<"id">> => Rec#server.id,
       <<"registration">> => Rec#server.registration,
       <<"readonly">> => Rec#server.readonly,
       <<"map">> => Rec#server.map,
       <<"bingKey">> => Rec#server.bingKey,
       <<"mapUrl">> => Rec#server.mapUrl,
       <<"language">> => Rec#server.language,
       <<"distanceUnit">> => Rec#server.distanceUnit,
       <<"speedUnit">> => Rec#server.speedUnit,
       <<"latitude">> => Rec#server.latitude,
       <<"longitude">> => Rec#server.longitude,
       <<"zoom">> => Rec#server.zoom
     };
to_map(permission, Rec) ->
    #{
       <<"id">> => Rec#permission.id,
       <<"userId">> => Rec#permission.userId,
       <<"deviceId">> => Rec#permission.deviceId
     };
to_map(user, Rec) ->
    #{
       <<"id">> => Rec#user.id,
       <<"name">> => Rec#user.name,
       <<"email">> => Rec#user.email,
       <<"readonly">> => Rec#user.readonly,
       <<"admin">> => Rec#user.admin,
       <<"map">> => Rec#user.map,
       <<"language">> => Rec#user.language,
       <<"distanceUnit">> => Rec#user.distanceUnit,
       <<"speedUnit">> => Rec#user.speedUnit,
       <<"latitude">> => Rec#user.latitude,
       <<"longitude">> => Rec#user.longitude,
       <<"zoom">> => Rec#user.zoom,
       <<"password">> => Rec#user.password,
       <<"hashPassword">> => Rec#user.hashPassword,
       <<"salt">> => Rec#user.salt
     };
to_map(device, Rec) ->
    #{
       <<"id">> => Rec#device.id,
       <<"name">> => Rec#device.name,
       <<"uniqueId">> => Rec#device.uniqueId,
       <<"status">> => Rec#device.status,
       <<"lastUpdate">> => Rec#device.lastUpdate,
       <<"positionId">> => Rec#device.positionId
     };
to_map(position, Rec) ->
    #{
       <<"id">> => Rec#position.id,
       <<"type">> => Rec#position.type,
       <<"protocol">> => Rec#position.protocol,
       <<"serverTime">> => Rec#position.serverTime,
       <<"deviceTime">> => Rec#position.deviceTime,
       <<"fixTime">> => Rec#position.fixTime,
       <<"deviceId">> => Rec#position.deviceId,
       <<"outdated">> => Rec#position.outdated,
       <<"valid">> => Rec#position.valid,
       <<"latitude">> => Rec#position.latitude,
       <<"longitude">> => Rec#position.longitude,
       <<"altitude">> => Rec#position.altitude,
       <<"speed">> => Rec#position.speed,
       <<"course">> => Rec#position.course,
       <<"address">> => Rec#position.address,
       <<"attributes">> => Rec#position.attributes
     }.

gen_uid() -> bson:unixtime_to_secs(bson:timenow()).

hash(V) -> em_password:hash(V).


%%test() ->
%%em_storage_mongo:create_device(#device{name = <<"auto5">>, uniqueId =  <<"005">>}).
%%em_storage_mongo:update_device(#device{id = 1471064827, status = <<"online">>}).
%%em_storage_mongo:delete_device(Device).

%%em_storage_mongo:create_user(#user{name = <<"auto5">>, email =  <<"auto5">>, password = <<"auto5">>}).
%%em_storage_mongo:create_permission(#permission{userId = 1, deviceId = 1}).
%%em_storage_mongo:update_server(#server{id = 1458329899, registration = false}).
%%em_storage_mongo:update_user(#user{id = 1471067648, name = <<"auto5">>, email =  <<"aaa@aaa.aaa">>, password = <<"auto5">>}).
%%em_storage_mongo:delete_user(#user{id = 1464751123}).