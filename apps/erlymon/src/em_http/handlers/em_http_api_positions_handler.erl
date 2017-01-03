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
-module(em_http_api_positions_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").
-include("em_records.hrl").

-record(qs_params, {deviceId = 0 :: integer(), from = <<"">> :: string(), to = <<"">> :: string()}).

%% GET http://demo.traccar.org/api/user/get?_dc=1436251203853&page=1&start=0&limit=25
%% {"success":true,"data":[]}
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
  case cowboy_session:get(user, Req) of
    {undefined, Req2} ->
      {ok, cowboy_req:reply(?STATUS_NOT_FOUND, Req2), Opts};
    {User, Req2} ->
      Method = cowboy_req:method(Req2),
      {ok, request(Method, Req2, User), Opts}
  end.

-spec request(Method :: binary(), Opts :: any(), User :: map()) -> cowboy_req:req().
request(?GET, Req, User) ->
  get_positions(Req, User);
request(_, Req, _) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, Req).

-spec get_positions(Req :: cowboy_req:req(), User :: map()) -> cowboy_req:req().
get_positions(Req, User) ->
  Qs = cowboy_req:parse_qs(Req),
  Result = emodel:from_proplist(Qs, #qs_params{}, [
    {<<"deviceId">>, optional, integer, #qs_params.deviceId, []},
    {<<"from">>, optional, string, #qs_params.from, []},
    {<<"to">>, optional, string, #qs_params.to, []}
  ]),
  case Result of
    {ok, Params} ->
          em_logger:info("Device ID: ~w, From: ~s, To: ~s", [Params#qs_params.deviceId, Params#qs_params.from, Params#qs_params.to]),
          %%Id = str_to_int(DeviceId),
          case em_permissions_manager:check_device(User#user.id, Params#qs_params.deviceId) of
            false ->
              cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Device access denied">>, Req);
            _ ->
              Language = cowboy_req:header(<<"Accept-Language">>, Req, <<"en_US">>),
              {ok, TimeFrom} = em_helper_time:parse(<<"%Y-%m-%dT%H:%M:%S.000Z">>, Params#qs_params.from),
              {ok, TimeTo} = em_helper_time:parse(<<"%Y-%m-%dT%H:%M:%S.000Z">>, Params#qs_params.to),
              {ok, Positions} = em_data_manager:get_positions(Params#qs_params.deviceId, TimeFrom, TimeTo),
              FixPositions = lists:map(fun(Position) ->
                  case em_geocoder:reverse(Position#position.latitude, Position#position.longitude, Language) of
                    {ok, Address} ->
                      %%em_logger:info("Language: ~s Address: ~s", [Language, Address]),
                      Position#position{address = Address};
                    _ -> Position
                  end
                end, Positions),
              cowboy_req:reply(?STATUS_OK, ?HEADERS, str(FixPositions), Req)
          end;
    _Reason ->
      cowboy_req:reply(?STATUS_UNKNOWN, [], <<"Invalid format">>, Req)
  end.


str(Recs) when is_list(Recs) ->
  em_logger:info("CONVERT RECORDS: ~w", [Recs]),
  em_json:encode(lists:map(fun(Rec) -> rec_to_map(Rec) end, Recs));
str(Rec) ->
  em_json:encode(rec_to_map(Rec)).

rec_to_map(Rec) ->
  {ok, ServerTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, Rec#position.serverTime),
  {ok, DeviceTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, Rec#position.deviceTime),
  {ok, FixTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, Rec#position.fixTime),
  #{
    <<"id">> => Rec#position.id,
    <<"type">> => Rec#position.type,
    <<"protocol">> => Rec#position.protocol,
    <<"serverTime">> => ServerTime,
    <<"deviceTime">> => DeviceTime,
    <<"fixTime">> => FixTime,
    <<"deviceId">> => Rec#position.deviceId,
    <<"outdated">> => Rec#position.outdated,
    <<"valid">> => Rec#position.valid,
    <<"latitude">> => Rec#position.latitude,
    <<"longitude">> => Rec#position.longitude,
    <<"altitude">> => Rec#position.altitude,
    <<"speed">> => Rec#position.speed,
    <<"course">> => Rec#position.course,
    <<"address">> => Rec#position.address,
    <<"attributes">> => Rec#position.attributes
  }.