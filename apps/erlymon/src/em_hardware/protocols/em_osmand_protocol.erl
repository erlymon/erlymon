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
-module(em_osmand_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").


-include("em_http.hrl").
-include("em_records.hrl").

%% API
-export([init/2]).

-record(osmand_message, {
  id = <<"">>:: string(),
  deviceId = <<"">>:: string(),
  timestamp = 0 :: integer(),
  lat = 0.0 :: float(),
  lon = 0.0 :: float(),
  speed = 0.0 :: float(),
  bearing = 0.0 :: float(),
  heading = 0.0 :: float(),
  altitude = 0.0 :: float(),
  batt = 0.0 :: float(),
  hdop = 0.0 :: float(),
  valid = false :: boolean()
}).
%% http://localhost:5055/?id=123456&lat={0}&lon={1}&timestamp={2}&hdop={3}&altitude={4}&speed={5}
%% id=353490069161244&timestamp=1440085518&lat=53.944019&lon=27.6468195&speed=0.0&bearing=0.0&altitude=0.0&batt=53.0
%% httpi request => http GET localhost:5055 id==123456789012345 timestamp==1440085518 lat==53.944019 lon==27.6468195 speed==0.0 bearing==0.0 altitude==0.0 batt==53.0
init(Req, Opts) ->
    io:format("OPTS: ~w", [Opts]),
    Method = cowboy_req:method(Req),
    em_logger:info("URL: ~s", [cowboy_req:url(Req)]),
    Qs = cowboy_req:parse_qs(Req),
    {ok, request(Method, Qs, Req), Opts}.

request(<<"GET">>, Qs, Req) ->
  case parse(Qs) of
    {ok, {Imei, PositionModel}} ->
      case em_data_manager:get_device_by_uid(Imei) of
        {error, _Reason} ->
          em_logger:info("[packet] unit: unknown device with imei = '~s'", [Imei]),
          cowboy_req:reply(400, [], <<"Permission denide.">>, Req);
        {ok, Object} ->
          Position = PositionModel#position{
            deviceId = Object#device.id,
            protocol = <<"osmand">>
          },
          em_logger:info("save message => unit: id = '~w' imei = '~s' position: ~w", [Object#device.id, Imei, Position]),
          em_data_manager:create_position(Object#device.id, Position),
          cowboy_req:reply(200, Req)
      end;
    _Reason ->
      cowboy_req:reply(400, [], <<"Invalid format.">>, Req)
  end;
request(_, Req, _) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, [], <<"Allowed GET request.">>, Req).


parse(Qs) ->
  Result = emodel:from_proplist(Qs, #osmand_message{}, [
    {<<"id">>, optional, string, #osmand_message.id, []},
    {<<"deviceId">>, optional, string, #osmand_message.deviceId, []},
    {<<"timestamp">>, required, integer, #osmand_message.timestamp, []}, %% seconds or ISO8604
    {<<"lat">>, required, float, #osmand_message.lat, []},
    {<<"lon">>, required, float, #osmand_message.lon, []},
    {<<"speed">>, required, float, #osmand_message.speed, []},
    {<<"bearing">>, optional, float, #osmand_message.bearing, []},
    {<<"heading">>, optional, float, #osmand_message.bearing, []},
    {<<"altitude">>, required, float, #osmand_message.altitude, []},
    {<<"batt">>, required, float, #osmand_message.batt, []},
    {<<"hdop">>, optional, float, #osmand_message.hdop, []},
    {<<"valid">>, optional, boolean, #osmand_message.valid, []}
  ]),
  case Result of
    {ok, Msg} ->
      Imei = parse_imei(Msg),
      Position = #position{
        deviceTime = Msg#osmand_message.timestamp, %% seconds
        latitude = Msg#osmand_message.lat,
        longitude = Msg#osmand_message.lon,
        speed = Msg#osmand_message.speed,
        course = parse_course(Msg),
        altitude = Msg#osmand_message.altitude,
        valid = Msg#osmand_message.valid,
        attributes = #{
          ?KEY_BATTERY => Msg#osmand_message.batt,
          ?KEY_HDOP => Msg#osmand_message.hdop
        }
      },
      {ok, {Imei, Position}};
    Reason ->
      Reason
  end.

parse_imei(#osmand_message{id = Id, deviceId = DeviceId}) when Id =:= <<"">> -> DeviceId;
parse_imei(#osmand_message{id = Id, deviceId = DeviceId}) when DeviceId =:= <<"">> -> Id.

parse_course(#osmand_message{bearing = Bearing, heading = Heading}) when Bearing =:= 0.0 -> Heading;
parse_course(#osmand_message{bearing = Bearing, heading = Heading}) when Heading =:= 0.0 -> Bearing.
