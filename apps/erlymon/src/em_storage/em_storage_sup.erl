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
-module(em_storage_sup).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(StorageType :: term(), StorageSettings :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(StorageType, StorageSettings) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [StorageType, StorageSettings]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([StorageType, StorageSettings]) ->
  em_logger:info("Storage type: ~w settings: ~w", [StorageType, StorageSettings]),

  SupFlags = #{strategy => one_for_one, intensity => 1000, period => 3600},

  ChildSpecs = [
    #{
      id => em_storage,
      start => {em_storage, start_link, do_storage_args(StorageType, StorageSettings)},
      restart => permanent,
      shutdown => 3000,
      type => worker,
      modules => [em_storage]
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_storage_args(mongodb, [Seed, Options, WorkerOptions]) ->
  Callback = fun(Prop = {Key, Value}) ->
    case Key of
      database -> {Key, list_to_binary(Value)};
      _ -> Prop
    end
             end,
  FixWorkerOptions = lists:map(Callback, WorkerOptions),
  [Seed, Options, FixWorkerOptions].