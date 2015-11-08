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
-module(em_http_api_register_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").

%% POST http://demo.traccar.org/api/register
%% REQUEST PAYLOAD:
%% {name: "assa", email: "assa@assa.com", password: "assa"}

%% ANSWER:
%% {"success":false,"error":"Duplicate entry 'demo@demo.com' for key 'email'"}
%% or
%% {"success":true}


-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  {ok, request(Method, Req), Opts}.

-spec request(Method :: binary(), Opts :: any()) -> cowboy_req:req().
request(?POST, Req) ->
  register(Req);
request(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, Req).

-spec register(Req :: cowboy_req:req()) -> cowboy_req:req().
register(Req) ->
  {ok, [{JsonBin, true}], Req2} = cowboy_req:body_qs(Req),
  User = em_json:decode(JsonBin),
  Res = em_data_manager:create_user(User),
  cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{<<"success">> => Res}), Req2).
