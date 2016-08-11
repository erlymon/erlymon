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
-module(em_http_api_permissions_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").
-include("em_records.hrl").

-spec init(Req::cowboy_req:req(), Opts::any()) -> {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
  case cowboy_session:get(user, Req) of
    {undefined, Req2} ->
      {ok, cowboy_req:reply(?STATUS_NOT_FOUND, Req2), Opts};
    {User, Req2} ->
      Method = cowboy_req:method(Req2),
      {ok, request(Method, Req2, User), Opts}
  end.

-spec request(Method::binary(), Req::cowboy_req:req(), User::map()) -> cowboy_req:req().
request(?POST, Req, User) ->
  add_permission(Req, User);
request(?DELETE, Req, User) ->
  remove_permission(Req, User);
request(_, Req, _) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, Req).

-spec add_permission(Req::cowboy_req:req(), User::map()) -> cowboy_req:req().
add_permission(Req, User) ->
  {ok, [{JsonBin, true}], Req2} = cowboy_req:body_qs(Req),
  %% {userId: 1458137425, deviceId: 1458137433}
  %%PermissionModel = em_json:decode(JsonBin),
  em_logger:info("ADD PERMISSION JSON: ~s", [JsonBin]),
  Result = emodel:from_map(em_json:decode(JsonBin), #permission{}, [
    {<<"userId">>, required, integer, #permission.userId, []},
    {<<"deviceId">>, required, integer, #permission.deviceId, []}
  ]),
  case Result of
    {ok, PermissionModel} ->
          case em_permissions_manager:check_admin(User#user.id) of
            false ->
              cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req);
            _ ->
              em_data_manager:link_device(PermissionModel),
              cowboy_req:reply(?STATUS_OK, Req2)
          end;
    _Reason ->
      cowboy_req:reply(?STATUS_UNKNOWN, [], <<"Invalid format">>, Req)
  end.

-spec remove_permission(Req::cowboy_req:req(), User::map()) -> cowboy_req:req().
remove_permission(Req, User) ->
  {ok, [{JsonBin, true}], Req2} = cowboy_req:body_qs(Req),
  %% {userId: 1458137425, deviceId: 1458137433}
  %%PermissionModel = em_json:decode(JsonBin),
  em_logger:info("REMOVE PERMISSION JSON: ~s", [JsonBin]),
  Result = emodel:from_map(em_json:decode(JsonBin), #permission{}, [
    {<<"userId">>, required, integer, #permission.userId, []},
    {<<"deviceId">>, required, integer, #permission.deviceId, []}
  ]),
  case Result of
    {ok, PermissionModel} ->
          case em_permissions_manager:check_admin(User#user.id) of
            false ->
              cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req);
            _ ->
              em_data_manager:unlink_device(PermissionModel),
              cowboy_req:reply(?STATUS_OK, Req2)
          end;
    _Reason ->
      cowboy_req:reply(?STATUS_UNKNOWN, [], <<"Invalid format">>, Req)
  end.