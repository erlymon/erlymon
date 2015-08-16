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
-module(em_http_api_login_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").

%% POST http://demo.traccar.org/api/login
%% FORM DATA:
%% email:assa@assa.com
%% password:assa

%% ANSWER:
%% {"success":true,"data":{"admin":false,"email":"assa@assa.com","name":"assa","id":623}}
%% or
%% {"success":false}

-spec init(Req::cowboy_req:req(), Opts::any()) -> {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    case cowboy_req:has_body(Req) of
        true ->
            {ok, request(Method, Req), Opts};
        false ->
            {ok, cowboy_req:reply(?STATUS_BAD_REQUEST, Req), Opts}
    end.

-spec request(Method::binary(), Opts::any()) -> cowboy_req:req().
request(?POST, Req) ->
    login(Req);
request(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, Req).

-spec login(Req::cowboy_req:req()) -> cowboy_req:req().
login(Req) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    Email = proplists:get_value(<<"email">>, PostVals),
    Password = proplists:get_value(<<"password">>, PostVals),
    case em_data_manager:check_user(Email, Password) of
        null ->
            cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{success => false}), Req2);
        User ->
            {ok, Req3} = cowboy_session:set(user, User, Req2),
            cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{success => true, data => maps:remove(<<"_id">>, User)}), Req3)
    end.
