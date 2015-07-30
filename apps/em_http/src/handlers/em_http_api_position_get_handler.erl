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
-module(em_http_api_position_get_handler).
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
    get_positions(Req);
request(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, Req).

-spec get_positions(Req::cowboy_req:req()) -> cowboy_req:req().
get_positions(Req) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{success => false}), Req2);
        {User, Req2} ->
            #{deviceId := DeviceId, from := From, to := To} = cowboy_req:match_qs([deviceId, from, to], Req),
            %em_logger:info("Device ID: ~d, From: ~w, To: ~w", [str_to_int(DeviceId), str_to_utc(From), str_to_utc(To)]),
            Id = str_to_int(DeviceId),
            case em_permissions_manager:check_device(maps:get(id, User), Id) of
                false ->
                    cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{success => false}), Req2);
                _ ->
                    Messages = em_data_manager:get_messages(Id, str_to_utc(From), str_to_utc(To)),
                    cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{success => true, data => Messages}), Req2)
            end
    end.

str_to_int(Str) ->
    list_to_integer(binary_to_list(Str)).


str_to_utc(Str) ->
    Date = iso8601:parse(Str),
    datetime_to_utc(Date).

datetime_to_utc({{Year, Month, Day}, Time}) ->
    BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
        (calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, Time}) - BaseDate) * 1000.
