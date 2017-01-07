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
-module(erlymon_app).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(_StartType :: any(), _StartArgs :: list()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    em_proc:init(),
    Res = erlymon_sup:start_link(),
    {ok, EmHardwareArgs} = application:get_env(erlymon, em_hardware),
    em_hardware:start(EmHardwareArgs),
    start_web(_StartType, _StartArgs),
    Res.

%%--------------------------------------------------------------------
-spec stop(_State :: any()) -> ok.
stop(_State) ->
    stop_web(_State),
    {ok, EmHardwareArgs} = application:get_env(erlymon, em_hardware),
    em_hardware:stop(EmHardwareArgs).

start_web(_StartType, _StartArgs) ->
    {ok, EmHttp} = application:get_env(erlymon, em_http),
    em_logger:info("Start web server port: ~w", [proplists:get_value(port, EmHttp)]),
    Dispatch = cowboy_router:compile(em_http_routes:get([{debug, proplists:get_value(debug, EmHttp)}])),
    cowboy:start_http(web, 100, [
                                 {port, proplists:get_value(port, EmHttp)},
                                 {timeout, proplists:get_value(timeout, EmHttp)}
                                ], [{env, [{dispatch, Dispatch}]}]).

%%--------------------------------------------------------------------
stop_web(_State) ->
    cowboy:stop_listener(web).
