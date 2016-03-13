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
-module(em_http_api_users_v1_handler).
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
request(?GET, Req) ->
    get_users(Req);
request(?POST, Req) ->
    add_user(Req);
request(?PUT, Req) ->
    update_user(Req);
request(?DELETE, Req) ->
    remove_user(Req);
request(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, [], <<"Allowed GET,POST,PUT,DELETE requests.">>, Req).

-spec get_users(Req::cowboy_req:req()) -> cowboy_req:req().
get_users(Req) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_NOT_FOUND, Req2);
        {User, Req2} ->
            em_logger:info("User: ~w", [maps:get(<<"id">>,User)]),
            case em_permissions_manager:check_admin(maps:get(<<"id">>, User)) of
                false ->
                    cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req2);
                _ ->
                    Users = em_data_manager:get_users(),
                    cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(Users), Req2)
            end
    end.

-spec add_user(Req::cowboy_req:req()) -> cowboy_req:req().
add_user(Req) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_NOT_FOUND, Req2);
        {User, Req2} ->
            {ok, [{JsonBin, true}], Req3} = cowboy_req:body_qs(Req2),
            UserModel = em_json:decode(JsonBin),
            case em_permissions_manager:check_admin(maps:get(<<"id">>, User)) of
                false ->
                    cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req2);
                _ ->
                    NewUser = em_data_manager:create_user(UserModel),
                    cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(NewUser), Req3)
            end
    end.
    
-spec remove_user(Req :: cowboy_req:req()) -> cowboy_req:req().
remove_user(Req) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_NOT_FOUND, Req2);
        {User, Req2} ->
            case em_permissions_manager:check_admin(maps:get(<<"id">>, User)) of
                false ->
                    cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req2);
                _ ->
                    {ok, [{JsonBin, true}], Req3} = cowboy_req:body_qs(Req2),
                    UserModel = em_json:decode(JsonBin),
                    em_data_manager:delete_user(UserModel),
                    cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(UserModel), Req3)
            end
    end.
    
-spec update_user(Req::cowboy_req:req()) -> cowboy_req:req().
update_user(Req) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_NOT_FOUND, Req2);
        {User, Req2} ->
            {ok, [{JsonBin, true}], Req3} = cowboy_req:body_qs(Req2),
            UserModel = em_json:decode(JsonBin),
            case check_permission(maps:get(<<"id">>, User), maps:get(<<"id">>, UserModel)) of
                false ->
                    cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req3);
                _ ->
                    em_data_manager:update_user(maps:remove(<<"o">>,UserModel)),
                    cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(UserModel), Req3)
            end
    end.



check_permission(CurrUserId, UpdateUserId) when CurrUserId == UpdateUserId ->
    true;
check_permission(CurrUserId, _) ->
    em_permissions_manager:check_admin(CurrUserId).
