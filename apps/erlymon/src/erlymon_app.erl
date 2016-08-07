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

-define(ACCEPTORS, 10).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    em_storage_sup:start_link(),
    em_data_manager:init(),
    em_geocoder_sup:start_link(),
    start_hardware(_StartType, _StartArgs),
    start_web(_StartType, _StartArgs),
    erlymon_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    stop_web(_State),
    stop_hardware(_State).

    
start_hardware(_StartType, _StartArgs) ->
  {ok, EmHardware} = application:get_env(erlymon, em_hardware),
  
  TcpServers = proplists:get_value(tcp_servers, EmHardware),
  lists:foreach(fun({PoolName, Options, Args}) ->
    em_logger:info("Start tcp server  ~w port: ~w",[PoolName, proplists:get_value(port, Options)]),
    {ok, _} = ranch:start_listener(PoolName, ?ACCEPTORS, ranch_tcp, Options, make_module_name(PoolName), [{protocol, PoolName}, Args])
  end, TcpServers),
  
  HttpServers = proplists:get_value(http_servers, EmHardware),
  lists:foreach(fun({Name, Options, Routes}) ->
    em_logger:info("Start http server ~w port: ~w", [Name, proplists:get_value(port, Options)]),
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_http(Name, 100, Options, [{env, [{dispatch, Dispatch}]}])
  end, HttpServers).

%%--------------------------------------------------------------------
stop_hardware(_State) ->
  {ok, EmHardware} = application:get_env(erlymon, em_hardware),
  
  TcpServers = proplists:get_value(tcp_servers, EmHardware),
  lists:foreach(fun({PoolName, _, _}) ->
    ranch:stop_listener(PoolName)
  end, TcpServers),
  
  HttpServers = proplists:get_value(http_servers, EmHardware),
  lists:foreach(fun({Name, _, _}) ->
    cowboy:stop_listener(Name)
  end, HttpServers),
  ok.

  
start_web(_StartType, _StartArgs) ->
  {ok, EmHttp} = application:get_env(erlymon, em_http),
  Web = proplists:get_value(web, EmHttp),

  em_logger:info("Start web server port: ~w", [proplists:get_value(port, Web)]),
  Dispatch = cowboy_router:compile(routes:get([{debug, proplists:get_value(debug, Web)}])),
  cowboy:start_http(web, 100, [
    {port, proplists:get_value(port, Web)},
    {timeout, proplists:get_value(timeout, Web)}
  ], [{env, [{dispatch, Dispatch}]}]).

%%--------------------------------------------------------------------
stop_web(_State) ->
  cowboy:stop_listener(web).

%%====================================================================
%% Internal functions
%%====================================================================
make_module_name(Name) ->
  list_to_atom("em_" ++ atom_to_list(Name) ++ "_protocol").
%%====================================================================
%% Internal functions
%%====================================================================
