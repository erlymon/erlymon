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
-module(em_http_api_devices_v1_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").

%% GET http://demo.traccar.org/api/device/get?_dc=1436251203853&page=1&start=0&limit=25
%% {"success":true,"data":[]}
-spec init(Req::cowboy_req:req(), Opts::any()) -> {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  {ok, request(Method, Req), Opts}.

-spec request(Method::binary(), Opts::any()) -> cowboy_req:req().
request(?GET, Req) ->
    get_devices(Req);
request(?POST, Req) ->
    add_device(Req);
request(?PUT, Req) ->
    update_device(Req);
request(?DELETE, Req) ->
    remove_device(Req);
request(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, [], <<"Allowed GET,POST,PUT,DELETE requests.">>, Req).

-spec get_devices(Req::cowboy_req:req()) -> cowboy_req:req().
get_devices(Req) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_NOT_FOUND, Req2);
        {User, Req2} ->
            Qs = cowboy_req:parse_qs(Req2),
            All = binary_to_atom(proplists:get_value(<<"all">>, Qs, <<"false">>), utf8),
            UserId = list_to_integer(binary_to_list(proplists:get_value(<<"userId">>, Qs, <<"0">>))),
	    case All of
	     true ->
                case em_permissions_manager:check_admin(maps:get(<<"id">>, User)) of
                    false ->
                        cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req2);
                    _ ->
                        cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(em_data_manager:get_all_devices()), Req2)
                end;
             false ->
                case em_permissions_manager:check_user(get_user_id(UserId, User), UserId) of
                    false ->
                        cowboy_req:reply(?STATUS_FORBIDDEN, Req2);
                    _ ->
                        cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(em_data_manager:get_devices(get_user_id(UserId, User))), Req2)
                end
            end
    end.

get_user_id(0, User) ->
    maps:get(<<"id">>, User);
get_user_id(UserId, _) ->
    UserId.

-spec add_device(Req::cowboy_req:req()) -> cowboy_req:req().
add_device(Req) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_NOT_FOUND, Req2);
        {User, Req2} ->
            {ok, [{JsonBin, true}], Req3} = cowboy_req:body_qs(Req2),
            DeviceModel = em_json:decode(JsonBin),
            Device = em_data_manager:create_device(DeviceModel),
            em_data_manager:link_device(maps:get(<<"id">>, User), maps:get(<<"id">>, Device)),
            cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(maps:remove(<<"_id">>, Device)), Req3)
    end.
    
-spec update_device(Req::cowboy_req:req()) -> cowboy_req:req().
update_device(Req) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_NOT_FOUND, Req2);
        {User, Req2} ->
            {ok, [{JsonBin, true}], Req3} = cowboy_req:body_qs(Req2),
            Device = em_json:decode(JsonBin),
            case em_permissions_manager:check_device(maps:get(<<"id">>, User), maps:get(<<"id">>, Device)) of
                false ->
                    cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Device access denied">>, Req3);
                true ->
                    em_data_manager:update_device(Device),
                    cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(Device), Req3)
            end
    end.

-spec remove_device(Req::cowboy_req:req()) -> cowboy_req:req().
remove_device(Req) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_NOT_FOUND, Req2);
        {User, Req2} ->
            {ok, [{JsonBin, true}], Req3} = cowboy_req:body_qs(Req2),
            Device = em_json:decode(JsonBin),
            case em_permissions_manager:check_device(maps:get(<<"id">>, User), maps:get(<<"id">>, Device)) of
                false ->
                    cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Device access denied">>, Req3);
                true ->
                    em_data_manager:delete_device(Device),
                    em_data_manager:unlink_device(maps:get(<<"id">>, Device)),
                    cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(maps:remove(<<"_id">>, Device)), Req3)
            end
    end.
