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
-module(em_hardware_app).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

-define(ACCEPTORS, 10).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  {ok, Servers} = application:get_env(em_hardware, servers),
  lists:foreach(fun({PoolName, Options, Args}) ->
    em_logger:info("Start hardware ~w port: ~w",[PoolName, proplists:get_value(port, Options)]),
    {ok, _} = ranch:start_listener(PoolName, ?ACCEPTORS, ranch_tcp, Options, make_module_name(PoolName), [{protocol, PoolName}, Args])
  end, Servers),
  em_hardware_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  {ok, Servers} = application:get_env(em_hardware, servers),
  lists:foreach(fun({PoolName, _, _}) ->
    ranch:stop_listener(PoolName)
  end, Servers),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
make_module_name(Name) ->
  list_to_atom("em_" ++ atom_to_list(Name) ++ "_protocol").
