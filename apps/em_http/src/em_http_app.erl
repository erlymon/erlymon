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
-module(em_http_app).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  %%ok = cowboy_session_config:set(cookie_options, [{path, <<"/">>}, {domain, <<".site.com">>}]),
  %%ok = cowboy_session_config:set([
  %%  {cookie_name, <<"new_cookie_name">>},
  %%  {expire, 86400}
  %%]),
  {ok, Servers} = application:get_env(em_http, servers),
  lists:foreach(fun({Name, Options, Routes}) ->
    em_logger:info("Start http server ~w port: ~w", [Name, proplists:get_value(port, Options)]),
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_http(Name, 100, Options, [{env, [{dispatch, Dispatch}]}])
  end, Servers),
  em_http_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  {ok, Servers} = application:get_env(em_http, servers),
  lists:foreach(fun({Name, _, _}) ->
    cowboy:stop_listener(Name)
  end, Servers),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
