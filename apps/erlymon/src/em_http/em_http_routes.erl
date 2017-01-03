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
-module(em_http_routes).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API
-export([get/1]).

-spec get(Opts :: list()) -> list().
get(Opts) ->
  [{'_', [
    {"/api/server", em_http_api_server_handler, []},
    {"/api/session", em_http_api_session_handler, []},
    {"/api/devices[/:id]", em_http_api_devices_handler, []},
    {"/api/users[/:id]", em_http_api_users_handler, []},
    {"/api/positions", em_http_api_positions_handler, []},
    {"/api/socket", em_http_api_socket_handler, []},
    {"/api/permissions", em_http_api_permissions_handler, []},
    {"/api/commands", em_http_api_commands_handler, []},
    {"/api/statistics", em_http_api_statistics_handler, []},
    {"/", cowboy_static, {priv_file, erlymon, io_lib:format("web/~s.html", [debug(proplists:get_value(debug, Opts))])}},
    {"/[...]", cowboy_static, {priv_dir, erlymon, "web/", [{mimetypes, cow_mimetypes, all}]}}
  ]}].

debug(true) -> "debug";
debug(false) -> "release".