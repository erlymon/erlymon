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
-ifndef(RECORDS_HRL).
-define(RECORDS_HRL, true).

-record(server, {
  id = 0 :: integer(),
  registration = true :: boolean(),
  readonly = false :: boolean(),
  map = <<"osm">> :: string(),
  bingKey = <<"">> :: string(),
  mapUrl = <<"">> :: string(),
  language = <<"en">> :: string(),
  distanceUnit  = <<"km">> :: string(),
  speedUnit = <<"km/h">> :: string(),
  latitude = 0 :: string(),
  longitude = 0 :: string(),
  zoom = 0 :: string()
}).

-record(user, {
  id = 0 :: integer(),
  name = <<"">> :: string(),
  email = <<"">> :: string(),
  readonly = false :: boolean(),
  admin = false  :: boolean(),
  map = <<"osm">> :: string(),
  language = <<"en">> :: string(),
  distanceUnit  = <<"km">> :: string(),
  speedUnit = <<"km/h">> :: string(),
  latitude = 0 :: string(),
  longitude = 0 :: string(),
  zoom = 0 :: string(),
  password = <<"">> :: string(),
  hashPassword = <<"">> :: string(),
  salt = <<"">> :: string()
}).

-record(device, {
  id = 0 :: integer(),
  name = <<"">> :: string(),
  uniqueId = <<"">> :: string(),
  status = <<"">> :: string(),
  lastUpdate = 0 :: integer(),
  positionId = 0 :: integer()
}).

-record(position, {
  id = 0 :: integer(),
  type = <<"">> :: string(),
  protocol = <<"">> :: string(),
  serverTime = 0 :: integer(),  %% seconds
  deviceTime = 0 :: integer(),  %% seconds
  fixTime = 0 :: integer(),  %% seconds
  deviceId = 0 :: integer(),
  outdated = false :: boolean(),
  valid = false :: boolean(),
  latitude = 0.0 :: float(),
  longitude = 0.0 :: float(),
  altitude = 0.0 :: float(),
  speed = 0.0 :: float(),
  course = 0.0 :: float(),
  address = <<"">> :: string(),
  attributes =#{} :: map()
}).

-record(command, {
  deviceId = 0 :: integer(),
  type = <<"">> :: string(),
  attributes = #{} :: map()
}).

-record(permission, {
  id = 0 :: integer(),
  userId = 0 :: integer(),
  deviceId = 0 :: integer()
}).

-record(event, {
  devices = [] :: list(),
  positions = [] :: list()
}).

-record(device_state, {
  deviceId = 0 :: integer(),
  protocol :: any(),
  socket :: any(),
  transport :: any(),
  timeout :: integer()|atom(),
  pattern :: any()
}).

%%  Device
-define(STATUS_UNKNOWN, <<"unknown">>).
-define(STATUS_ONLINE, <<"online">>).
-define(STATUS_OFFLINE, <<"offline">>).

%% Words separated by dashes (word-second-third)
-define(KEY_INDEX, <<"index">>).
-define(KEY_HDOP, <<"hdop">>).
-define(KEY_SATELLITES, <<"sat">>).
-define(KEY_GSM, <<"gsm">>).
-define(KEY_GPS, <<"gps">>).
-define(KEY_EVENT, <<"event">>).
-define(KEY_ALARM, <<"alarm">>).
-define(KEY_STATUS, <<"status">>).
-define(KEY_ODOMETER, <<"odometer">>).
-define(KEY_HOURS, <<"hours">>).
-define(KEY_INPUT, <<"input">>).
-define(KEY_OUTPUT, <<"output">>).
-define(KEY_POWER, <<"power">>).
-define(KEY_BATTERY, <<"battery">>).
-define(KEY_MCC, <<"mcc">>).
-define(KEY_MNC, <<"mnc">>).
-define(KEY_LAC, <<"lac">>).
-define(KEY_CELL, <<"cell">>).
-define(KEY_CID, <<"cid">>).
-define(KEY_FUEL, <<"fuel">>).
-define(KEY_RFID, <<"rfid">>).
-define(KEY_VERSION, <<"version">>).
-define(KEY_TYPE, <<"type">>).
-define(KEY_IGNITION, <<"ignition">>).
-define(KEY_FLAGS, <<"flags">>).
-define(KEY_CHARGE, <<"charge">>).
-define(KEY_IP, <<"ip">>).
-define(KEY_ARCHIVE, <<"archive">>).
-define(KEY_DISTANCE, <<"distance">>).
-define(KEY_RPM, <<"rpm">>).
-define(KEY_VIN, <<"vin">>).
-define(KEY_APPROXIMATE, <<"approximate">>).
-define(KEY_THROTTLE, <<"throttle">>).

-define(KEY_OBD_SPEED, <<"obd-speed">>).
-define(KEY_OBD_ODOMETER, <<"obd-odometer">>).

%% Starts with 1 not 0
-define(PREFIX_TEMP, <<"temp">>).
-define(PREFIX_ADC, <<"adc">>).
-define(PREFIX_IO, <<"io">>).
-define(PREFIX_COUNT, <<"count">>).


%% COMMANT TYPES AND ATTR FIELDS

-define(TYPE_POSITION_SINGLE, <<"positionSingle">>).
-define(TYPE_POSITION_PERIODIC, <<"positionPeriodic">>).
-define(TYPE_POSITION_STOP, <<"positionStop">>).
-define(TYPE_ENGINE_STOP, <<"engineStop">>).
-define(TYPE_ENGINE_RESUME, <<"engineResume">>).
-define(TYPE_ALARM_ARM, <<"alarmArm">>).
-define(TYPE_ALARM_DISARM, <<"alarmDisarm">>).
-define(TYPE_SET_TIMEZONE, <<"setTimezone">>).
-define(TYPE_REQUEST_PHOTO, <<"requestPhoto">>).
-define(TYPE_REBOOT_DEVICE, <<"rebootDevice">>).
-define(TYPE_MOVEMENT_ALARM, <<"movementAlarm">>).

-define(KEY_UNIQUE_ID, <<"uniqueId">>).
-define(KEY_FREQUENCY, <<"frequency">>).
-define(KEY_TIMEZONE, <<"timezone">>).
-define(KEY_DEVICE_PASSWORD, <<"devicePassword">>).
-define(KEY_RADIUS, <<"radius">>).

-endif. % RECORDS_HRL
