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
-module(em_http_api_positions_v1_handler).
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
            cowboy_req:reply(?STATUS_NOT_FOUND, Req2);
        {User, Req2} ->
            %%#{deviceId := DeviceId, from := From, to := To} = cowboy_req:match_qs([deviceId, from, to], Req),
            Qs = cowboy_req:parse_qs(Req2),
            DeviceId = list_to_integer(binary_to_list(proplists:get_value(<<"deviceId">>, Qs, <<"0">>))),
            From = proplists:get_value(<<"from">>, Qs, <<"">>),
            To = proplists:get_value(<<"to">>, Qs, <<"">>),
            em_logger:info("Device ID: ~w, From: ~s, To: ~s", [DeviceId, From, To]),
            %%Id = str_to_int(DeviceId),
            case em_permissions_manager:check_device(maps:get(<<"id">>, User), DeviceId) of
                false ->
                    cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Device access denied">>, Req2);
                _ ->
                    {ok, TimeFrom} = em_helper_time:parse(<<"%Y-%m-%dT%H:%M:%S.000Z">>, From),
                    {ok, TimeTo} = em_helper_time:parse(<<"%Y-%m-%dT%H:%M:%S.000Z">>, To),
                    Messages = em_data_manager:get_messages(DeviceId, TimeFrom * 1000, TimeTo * 1000),
                    cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(Messages), Req2)
            end
    end.

str_to_int(Str) ->
    list_to_integer(binary_to_list(Str)).
