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
-module(em_http_api_commands_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").

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
  add_command(Req, User);
request(_, Req, _) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, Req).

-spec add_command(Req::cowboy_req:req(), User::map()) -> cowboy_req:req().
add_command(Req, User) ->
  {ok, [{JsonBin, true}], Req2} = cowboy_req:body_qs(Req),
  CommandModel = em_json:decode(JsonBin),
  case em_permissions_manager:check_device(maps:get(<<"id">>, User), maps:get(<<"deviceId">>, CommandModel)) of
    false ->
      cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req2);
    _ ->
      cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"TODO: need implement">>, Req2)
  end.