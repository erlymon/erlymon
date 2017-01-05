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
-module(erlymon_sup).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init(Args :: list()) -> {ok, tuple()}.
init([]) ->
    {ok, EmStorageEnv} = application:get_env(erlymon, em_storage),
    StorageType = proplists:get_value(type, EmStorageEnv),
    StorageSettings = proplists:get_value(settings, EmStorageEnv),

    {ok, EmGeocoderEnv} = application:get_env(erlymon, em_geocoder),
    GeocoderType = proplists:get_value(type, EmGeocoderEnv),
    GeocoderSettings = proplists:get_value(settings, EmGeocoderEnv),

    SupFlags = #{strategy => one_for_one, intensity => 1000, period => 3600},
    ChildSpecs = [
        #{
            id => em_regexp_sup,
            start => {em_regexp_sup, start_link, []},
            restart => permanent,
            shutdown => 3000,
            type => supervisor,
            modules => dynamic
        },
        #{
            id => em_storage_sup,
            start => {em_storage_sup, start_link, [StorageType, StorageSettings]},
            restart => permanent,
            shutdown => 3000,
            type => supervisor,
            modules => dynamic
        },
        #{
            id => em_manager_sup,
            start => {em_manager_sup, start_link, []},
            restart => permanent,
            shutdown => 3000,
            type => supervisor,
            modules => dynamic
        },
        #{
            id => em_geocoder_sup,
            start => {em_geocoder_sup, start_link, [GeocoderType, GeocoderSettings]},
            restart => permanent,
            shutdown => 3000,
            type => supervisor,
            modules => dynamic
        },
        #{
            id => em_stats,
            start => {em_stats, start_link, []},
            restart => permanent,
            shutdown => 3000,
            type => worker,
            modules => [em_stats]
        },
        #{
            id => em_timer,
            start => {em_timer, start_link, []},
            restart => permanent,
            shutdown => 3000,
            type => worker,
            modules => [em_timer]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
