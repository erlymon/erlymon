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
-module(em_http).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-export([
  start/1,
  stop/1
]).

-spec start(Args :: any()) -> any().
start(Args) ->
  em_logger:info("Start web server port: ~w", [proplists:get_value(port, Args)]),
  ok = cowboy_session_config:set([
  	{expire, 86400}
  ]),
  Dispatch = em_http_routes:compile([{debug, proplists:get_value(debug, Args)}]),
  Options = [
    {port, proplists:get_value(port, Args)}
    %%{timeout, proplists:get_value(timeout, Args)}
  ],
  {ok, _} = cowboy:start_clear(http, Options, #{
		env => #{dispatch => Dispatch}
	}).

-spec stop(Args :: any()) -> any().
stop(_Args) ->
  cowboy:stop_listener(http).
