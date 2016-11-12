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
-module(em_geocoder_sup).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec(start_link(GeocoderType :: term(), GeocoderSettings :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(GeocoderType, GeocoderSettings) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [GeocoderType, GeocoderSettings]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([GeocoderType, GeocoderSettings]) ->
    SupFlags = #{strategy => one_for_all, intensity => 1000, period => 3600},
    ChildSpecs = [
        #{
            id => em_geocoder,
            start => {em_geocoder, start_link, [GeocoderType, GeocoderSettings]},
            restart => permanent,
            shutdown => 3000,
            type => worker,
            modules => dynamic
        }
    ],
    {ok, {SupFlags, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
