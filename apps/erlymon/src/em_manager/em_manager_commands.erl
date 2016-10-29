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
-module(em_manager_commands).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(gen_server).

-include("em_records.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0]).

-export([
  get/0,
  get/1,
  link/1,
  unlink/1,
  execute/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {cache}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(get() ->
  {ok, [#device_state{}]} | {error, string()}).
get() ->
  gen_server:call(?SERVER, {get}).

-spec(get(DeviceId :: integer()) ->
  {ok, DeviceState :: #device_state{}} | {error, string()}).
get(DeviceId) ->
  gen_server:call(?SERVER, {get, DeviceId}).

-spec(link(DeviceState :: #device_state{}) ->
  {ok, DeviceState :: #device_state{}} | {error, string()}).
link(DeviceState) ->
  gen_server:call(?SERVER, {link, DeviceState}).

-spec(unlink(DeviceState :: #device_state{}) ->
  {ok, DeviceState :: #device_state{}} | {error, string()}).
unlink(DeviceState) ->
  gen_server:call(?SERVER, {unlink, DeviceState}).

-spec(execute(Command :: #command{}) ->
  {ok, Command :: #command{}} | {error, string()}).
execute(Command) ->
  gen_server:call(?SERVER, {execute, Command}).


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
  em_logger:info("Init commands manager"),
  Cache = ets:new(device_state, [set, private, {keypos, 2}]),
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
  do_get_devices_state(State);
handle_call({get, Id}, _From, State) when is_integer(Id) ->
  do_get_device_state_by_id(State, Id);
handle_call({link, DeviceState}, _From, State) ->
  do_link_device_state(State, DeviceState);
handle_call({unkink, DeviceState}, _From, State) ->
  do_unlink_device_state(State, DeviceState);
handle_call({execute, Command}, _From, State) ->
  do_execute_command(State, Command);
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
terminate(_Reason, _State) ->
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
do_get_devices_state(State = #state{cache = Cache}) ->
  {reply, {ok, ets:tab2list(Cache)}, State}.

do_get_device_state_by_id(State = #state{cache = Cache}, Id) ->
  case ets:lookup(Cache, Id) of
    [] ->
      {reply, {error, <<"Access is denied">>}, State};
    [Item|_] ->
      {reply, {ok, Item}, State}
  end.

do_link_device_state(State = #state{cache = Cache}, DeviceState) ->
  case ets:insert_new(Cache, DeviceState) of
    true ->
      {reply, {ok, DeviceState}, State};
    false ->
      {reply, {error, <<"Error link device state">>}, State}
  end.

do_unlink_device_state(State = #state{cache = Cache}, DeviceState) ->
  case ets:delete(Cache, DeviceState#device_state.deviceId) of
    true ->
      {reply, {ok, DeviceState}, State};
    false -> {reply, {error, <<"Error unlink device state">>}, State}
  end.

do_execute_command(State = #state{cache = Cache}, Command) ->
  case ets:lookup(Cache, Command#command.deviceId) of
    [] ->
      {reply, {error, <<"Access is denied">>}, State};
    [DeviceState|_] ->
      {reply, {ok, DeviceState}, State}
  end.
