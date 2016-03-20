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
-module(em_http_api_device_link_v0_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").

-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
  case cowboy_session:get(user, Req) of
    {undefined, Req2} ->
      cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{<<"success">> => false}), Req2);
    {User, Req2} ->
      Method = cowboy_req:method(Req2),
      {ok, request(Method, Req2, User), Opts}
  end.

-spec request(Method :: binary(), Opts :: any(), User::map()) -> cowboy_req:req().
request(?POST, Req, User) ->
  add_permission(Req, User);
request(_, Req, _) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, Req).

-spec add_permission(Req :: cowboy_req:req(), User::map()) -> cowboy_req:req().
add_permission(Req, User) ->
  case em_permissions_manager:check_admin(maps:get(<<"id">>, User)) of
    false ->
      cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{<<"success">> => false}), Req);
    _ ->
      {ok, [{JsonBin, true}], Req2} = cowboy_req:body_qs(Req),
      %% {userId: 1458137425, deviceId: 1458137433}
      PermissionModel = em_json:decode(JsonBin),
      em_data_manager:link_device(maps:get(<<"userId">>, PermissionModel), maps:get(<<"deviceId">>, PermissionModel)),
      cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{<<"success">> => true}), Req2)
  end.
