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
-module(em_manager_permissions).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(gen_server).

-include("em_records.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0]).

-export([
  get/0,
  get/2,
  get_by_user_id/1,
  get_by_device_id/1,
  create/1,
  delete/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {cache :: any()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(get() -> {ok, [Rec :: #device_permission{}]} | {error, string()}).
get() ->
  gen_server:call(?SERVER, {get}).

-spec(get(UserId :: integer(), DeviceId :: integer()) -> {ok, [Rec :: #device_permission{}]} | {error, string()}).
get(UserId, DeviceId) ->
  gen_server:call(?SERVER, {get, UserId, DeviceId}).

-spec(get_by_user_id(Id :: integer()) -> {ok, [Rec :: #device_permission{}]} | {error, string()}).
get_by_user_id(Id) ->
  gen_server:call(?SERVER, {get_by_user_id, Id}).

-spec(get_by_device_id(Id :: integer()) -> {ok, [Rec :: #device_permission{}]} | {error, string()}).
get_by_device_id(Id) ->
  gen_server:call(?SERVER, {get_by_device_id, Id}).

-spec(create(Permission :: #device_permission{}) -> {ok, #device_permission{}} | {error, string()}).
create(Permission) ->
  gen_server:call(?SERVER, {create, Permission}).

-spec(delete(Permission :: #device_permission{}) -> {ok, #device_permission{}} | {error, string()}).
delete(Permission) ->
  gen_server:call(?SERVER, {delete, Permission}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
  em_logger:info("Init permissions manager"),
  Cache = ets:new(permissions, [set, private, {keypos, 2}]),
  {ok, Items} = em_storage:get_permissions(),
  lists:foreach(fun(Item) ->
    ets:insert_new(Cache, Item)
                end, Items),
  em_logger:info("Loaded ~w permission(s)", [length(ets:tab2list(Cache))]),
  {ok, #state{cache = Cache}}.

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
handle_call({get}, _From, State) ->
  do_get_permissions(State);
handle_call({get, UserId, DeviceId}, _From, State) ->
  do_get_permission(State, UserId, DeviceId);
handle_call({get_by_user_id, Id}, _From, State) when is_integer(Id) ->
  do_get_permissions_by_user_id(State, Id);
handle_call({get_by_device_id, Id}, _From, State) when is_integer(Id) ->
  do_get_permissions_by_device_id(State, Id);
handle_call({create, Permission}, _From, State) ->
  do_create_permission(State, Permission);
handle_call({delete, Permission}, _From, State) ->
  do_delete_permission(State, Permission);
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
terminate(_Reason, #state{cache = Cache}) ->
  ets:delete(Cache),
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
do_get_permissions(State = #state{cache = Cache}) ->
  {reply, {ok, ets:tab2list(Cache)}, State}.

do_get_permission(State = #state{cache = Cache}, SearchUserId, SearchDeviceId) ->
  Match = ets:fun2ms(fun(Permission = #device_permission{userId = UserId, deviceId = DeviceId}) when (UserId =:= SearchUserId) and (DeviceId =:= SearchDeviceId) -> Permission end),
  case ets:select(Cache, Match) of
    [] ->
      {reply, {error, <<"Not find permission">>}, State};
    [Item|_] ->
      {reply, {ok, Item}, State}
  end.

do_get_permissions_by_user_id(State = #state{cache = Cache}, SearchUserId) ->
  Match = ets:fun2ms(fun(Permission = #device_permission{userId = UserId}) when UserId =:= SearchUserId -> Permission end),
  {reply, {ok, ets:select(Cache, Match)}, State}.

do_get_permissions_by_device_id(State = #state{cache = Cache}, SearchDeviceId) ->
  Match = ets:fun2ms(fun(Permission = #device_permission{deviceId = DeviceId}) when DeviceId =:= SearchDeviceId -> Permission end),
  {reply, {ok, ets:select(Cache, Match)}, State}.

do_create_permission(State = #state{cache = Cache}, PermissionModel) ->
  case em_storage:create_permission(PermissionModel) of
    {ok, Permission} ->
      case ets:insert_new(Cache, Permission) of
        true ->
          {reply, {ok, Permission}, State};
        false ->
          {reply, {error, <<"Error sync in permissions cache">>}, State}
      end;
    Reason ->
      {reply, Reason, State}
  end.

do_delete_permission(State = #state{cache = Cache}, PermissionModel) ->
  case em_storage:delete_permission(PermissionModel) of
    {ok, Permission} ->
      Match = ets:fun2ms(fun(#device_permission{userId = UserId, deviceId = DeviceId}) when (UserId =:= Permission#device_permission.userId) and (DeviceId =:= Permission#device_permission.deviceId) -> true end),
      case ets:select_delete(Cache, Match) of
        0 ->
          {reply, {error, <<"Error sync in permissions cache">>}, State};
        _ ->
          {reply, {ok, Permission}, State}
      end;
    Reason ->
      {reply, Reason, State}
  end.