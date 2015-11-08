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

-module(em_storage_message).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API
-export([
  create/3,
  get/1,
  get/2,
  get/3
]).

%%// message
%%{
%%  id: {type: integer},
%%  protocol: {type: string},
%%  deviceId: {type: integer},

%%  serverTime: {type: integer},
%%  deviceTime: {type: integer},
%%  fixTime: {type: integer},

%%  valid: {type: integer},

%%  latitude: {type: float},
%%  longitude: {type: float},
%%  altitude: {type: float},

%%  speed: {type: float},
%%  course: {type: float}

%%  /*
%%   KEY_INDEX = "index";
%%   KEY_HDOP = "hdop";
%%   KEY_SATELLITES = "sat";
%%   KEY_GSM = "gsm";
%%   KEY_GPS = "gps";
%%   KEY_EVENT = "event";
%%   KEY_ALARM = "alarm";
%%   KEY_STATUS = "status";
%%   KEY_ODOMETER = "odometer";
%%   KEY_INPUT = "input";
%%   KEY_OUTPUT = "output";
%%   KEY_POWER = "power";
%%   KEY_BATTERY = "battery";
%%   KEY_MCC = "mcc";
%%   KEY_MNC = "mnc";
%%   KEY_LAC = "lac";
%%   KEY_CELL = "cell";
%%   KEY_FUEL = "fuel";
%%   KEY_RFID = "rfid";
%%   KEY_VERSION = "version";
%%   KEY_TYPE = "type";
%%   KEY_IGNITION = "ignition";
%%   KEY_FLAGS = "flags";
%%   KEY_CHARGE = "charge";
%%   KEY_IP = "ip";

%%  // Starts with 1 not 0
%%   PREFIX_TEMP = "temp";
%%   PREFIX_ADC = "adc";
%%   PREFIX_IO = "io";
%%   PREFIX_COUNT = "count";
%%*/
%%}


%%        { name: 'id', type: 'int' },
%%        { name: 'protocol', type: 'string' },
%%        { name: 'deviceId', type: 'int' },
%%        { name: 'serverTime', type: 'date' },
%%        { name: 'deviceTime', type: 'date' },
%%        { name: 'fixTime', type: 'date' },
%%        { name: 'valid', type: 'boolean' },
%%        { name: 'latitude', type: 'float' },
%%        { name: 'longitude', type: 'float' },
%%        { name: 'altitude', type: 'float' },
%%        { name: 'speed', type: 'float' },
%%        { name: 'course', type: 'float' },
%%        { name: 'address', type: 'string' },
%%        { name: 'other', type: 'string' }

create(DeviceId, Protocol, MessageParams) ->
    ServerTime = timestamp(),
    DeviceTime = maps:get(<<"deviceTime">>, MessageParams),
    Latitude = maps:get(<<"latitude">>, MessageParams),
    Longitude = maps:get(<<"longitude">>, MessageParams),
    Address = em_geocoder:geocode(Latitude, Longitude, en),
    MessageModel = maps:merge(MessageParams, #{
      <<"id">> => bson:unixtime_to_secs(bson:timenow()),
      <<"serverTime">> => ServerTime,
      <<"fixTime">> => fix_time(ServerTime, DeviceTime),
      <<"deviceTime">> => DeviceTime,
      <<"deviceId">> => DeviceId,
      <<"protocol">> => Protocol,
      <<"address">> => Address
     }),
    em_logger:info("Message: ~w", [MessageModel]),
    em_storage:insert(messages, MessageModel).



fix_time(ServerTime, DeviceTime) when ServerTime < DeviceTime ->
    ServerTime;
fix_time(_, DeviceTime) ->
    DeviceTime.

%%--------------------------------------------------------------------
%% @doc Calc current utc time
%% @spec timestamp() -> integer().
%% @end
%%--------------------------------------------------------------------
timestamp() ->
  timer:now_diff(os:timestamp(), {0, 0, 0}).


get(SearchSpec) ->
    em_storage:find_one(messages, SearchSpec, [{projector, #{<<"_id">> => false}}]).
    
get(DeviceId, DeviceTime) ->
    em_storage:find_one(messages, #{<<"deviceId">> => DeviceId, <<"deviceTime">> => DeviceTime}, [{projector, #{<<"_id">> => false}}]).
    
get(DeviceId, TimeFrom, TimeTo) ->
    em_storage:find(messages, #{<<"deviceId">> => DeviceId, <<"fixTime">> => {'$gte', TimeFrom, '$lte', TimeTo}}, [{projector, #{<<"_id">> => false}}]).
