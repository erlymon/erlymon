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
-module(em_http_api_session_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").
-include("em_records.hrl").

%% GET http://demo.traccar.org/api/session?_dc=1436250980109
%% QUERY STRING PARAM:
%% _dc:1436250979884

%% ANSWER:
%% 200 {"id":12371,"name":"asd","email":"asd@asd.con","readonly":false,"admin":false,"map":null,"language":null,"distanceUnit":null,"speedUnit":null,"latitude":0.0,"longitude":0.0,"zoom":0,"password":null}
%% or
%% 404

-spec init(Req::cowboy_req:req(), Opts::any()) -> {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    {ok, request(Method, Req), Opts}.

-spec request(Method::binary(), Opts::any()) -> cowboy_req:req().
request(?GET, Req) ->
    get_session(Req);
request(?POST, Req) ->
    add_session(Req);
request(?DELETE, Req) ->
    remove_session(Req);
request(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, Req).

-spec get_session(Req::cowboy_req:req()) -> cowboy_req:req().
get_session(Req) ->
    %% {"id":12371,"name":"asd","email":"asd@asd.con","readonly":false,"admin":false,"map":null,"language":null,"distanceUnit":null,"speedUnit":null,"latitude":0.0,"longitude":0.0,"zoom":0,"password":null}
    %% or
    %% 404
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_NOT_FOUND, Req2);
        {User, Req2} ->
            cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(User), Req2)
    end.

-spec add_session(Req::cowboy_req:req()) -> cowboy_req:req().
add_session(Req) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    Result = emodel:from_proplist(PostVals, #user{}, [
        {<<"email">>, required, string, #user.email, []},
        {<<"password">>, required, string, #user.password, []}
    ]),
    case Result of
        {ok, #user{email =  Email, password = Password}} ->
                case em_data_manager:check_user(Email, Password) of
                    null ->
                        cowboy_req:reply(?STATUS_UNAUTHORIZED, Req2);
                    User ->
                        {ok, Req3} = cowboy_session:set(user, User, Req2),
                        cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(User), Req3)
                end;
      {error, _Reason} ->
                %% Encode end set error
                cowboy_req:reply(?STATUS_UNAUTHORIZED, Req2)
    end.

-spec remove_session(Req::cowboy_req:req()) -> cowboy_req:req().
remove_session(Req) ->
    {ok, Req2} = cowboy_session:expire(Req),
    cowboy_req:reply(?STATUS_OK, Req2).
