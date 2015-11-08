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
-module(em_http_api_user_add_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").

%% GET http://demo.traccar.org/api/user/get?_dc=1436251203853&page=1&start=0&limit=25
%% {"success":true,"data":[]}
-spec init(Req::cowboy_req:req(), Opts::any()) -> {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
  Method = cowboy_req:method(Req),
      {ok, request(Method, Req), Opts}.

-spec request(Method::binary(), Opts::any()) -> cowboy_req:req().
request(?POST, Req) ->
    add_user(Req);
request(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, Req).

-spec add_user(Req::cowboy_req:req()) -> cowboy_req:req().
add_user(Req) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{success => false}), Req2);
        {User, Req2} ->
            {ok, [{JsonBin, true}], Req3} = cowboy_req:body_qs(Req2),
            UserModel = em_json:decode(JsonBin),
            case em_permissions_manager:check_admin(maps:get(<<"id">>, User)) of
                false ->
                    cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{<<"success">> => false}), Req3);
                _ ->
                    case em_data_manager:create_user(UserModel) of
                        null ->
                            cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{<<"success">> => false}), Req3);
                        NewUser ->
                            cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{<<"success">> => true, <<"data">> => NewUser}), Req3)
                    end
            end
    end.
