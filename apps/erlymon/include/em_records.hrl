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
  speedUnit = <<"kmh">> :: string(),
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
  speedUnit = <<"kmh">> :: string(),
  latitude = 0 :: string(),
  longitude = 0 :: string(),
  zoom = 0 :: string(),
  password = <<"">> :: string(),
  hashPassword = <<"">> :: string(),
  salt = <<"">> :: string()
}).

-record(device, {
  id = 0 :: integer(),
  name = <<"">> :: string() | atom(),
  uniqueId = <<"">> :: string() | atom(),
  status = <<"">> :: string() | atom(),
  lastUpdate = 0 :: integer() | atom(),
  positionId = 0 :: integer() | atom()
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

-record(device_permission, {
  id = 0 :: integer(),
  userId = 0 :: integer(),
  deviceId = 0 :: integer()
}).

-record(event, {
  devices = [] :: list(),
  positions = [] :: list()
}).

-record(address, {
  postcode :: string(),
  country :: string(),
  state :: string(),
  district :: string(),
  settlement :: string(),
  suburb :: string(),
  street :: string(),
  house :: string()
}).

-record(statistics, {
  node :: atom(),
  usersCounter :: integer(),
  devicesCounter :: integer()
}).

%%  Device
-define(STATUS_UNKNOWN, <<"unknown">>).
-define(STATUS_ONLINE, <<"online">>).
-define(STATUS_OFFLINE, <<"offline">>).

%% Words separated by dashes (word-second-third)
-define(KEY_ORIGINAL, <<"raw">>).
-define(KEY_INDEX, <<"index">>).
-define(KEY_HDOP, <<"hdop">>).
-define(KEY_VDOP, <<"vdop">>).
-define(KEY_PDOP, <<"pdop">>).
-define(KEY_SATELLITES, <<"sat">>). %% in use
-define(KEY_SATELLITES_VISIBLE, <<"satVisible">>).
-define(KEY_RSSI, <<"rssi">>).
-define(KEY_GPS, <<"gps">>).
-define(KEY_EVENT, <<"event">>).
-define(KEY_ALARM, <<"alarm">>).
-define(KEY_STATUS, <<"status">>).
-define(KEY_ODOMETER, <<"odometer">>). %% meters
-define(KEY_TRIP_ODOMETER, <<"tripOdometer">>).
-define(KEY_HOURS, <<"hours">>).
-define(KEY_INPUT, <<"input">>).
-define(KEY_OUTPUT, <<"output">>).
-define(KEY_POWER, <<"power">>).
-define(KEY_BATTERY, <<"battery">>).
-define(KEY_FUEL, <<"fuel">>).
-define(KEY_FUEL_CONSUMPTION, <<"fuelConsumption">>).
-define(KEY_RFID, <<"rfid">>).
-define(KEY_VERSION_FW, <<"versionFw">>).
-define(KEY_VERSION_HW, <<"versionHw">>).
-define(KEY_TYPE, <<"type">>).
-define(KEY_IGNITION, <<"ignition">>).
-define(KEY_FLAGS, <<"flags">>).
-define(KEY_CHARGE, <<"charge">>).
-define(KEY_IP, <<"ip">>).
-define(KEY_ARCHIVE, <<"archive">>).
-define(KEY_DISTANCE, <<"distance">>). %% meters
-define(KEY_TOTAL_DISTANCE, <<"totalDistance">>).
-define(KEY_RPM, <<"rpm">>).
-define(KEY_VIN, <<"vin">>).
-define(KEY_APPROXIMATE, <<"approximate">>).
-define(KEY_THROTTLE, <<"throttle">>).
-define(KEY_MOTION, <<"motion">>).
-define(KEY_ARMED, <<"armed">>).
-define(KEY_ACCURACY, <<"accuracy">>).
-define(KEY_GEOFENCE, <<"geofence">>).
-define(KEY_ACCELERATION, <<"acceleration">>).
-define(KEY_DEVICE_TEMP, <<"deviceTemp">>).

-define(KEY_DTCS, <<"dtcs">>).
-define(KEY_OBD_SPEED, <<"obdSpeed">>).
-define(KEY_OBD_ODOMETER, <<"obdOdometer">>).

-define(KEY_RESULT, <<"result">>).

%% Starts with 1 not 0
-define(PREFIX_TEMP, <<"temp">>).
-define(PREFIX_ADC, <<"adc">>).
-define(PREFIX_IO, <<"io">>).
-define(PREFIX_COUNT, <<"count">>).


%% COMMANT TYPES AND ATTR FIELDS

-define(TYPE_CUSTOM, <<"custom">>).
-define(TYPE_IDENTIFICATION, <<"deviceIdentification">>).
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
-define(TYPE_SEND_SMS, <<"sendSms">>).
-define(TYPE_SEND_USSD, <<"sendUssd">>).
-define(TYPE_SOS_NUMBER, <<"sosNumber">>).
-define(TYPE_SILENCE_TIME, <<"silenceTime">>).
-define(TYPE_SET_PHONEBOOK, <<"setPhonebook">>).
-define(TYPE_VOICE_MESSAGE, <<"voiceMessage">>).
-define(TYPE_OUTPUT_CONTROL, <<"outputControl">>).
-define(TYPE_VOICE_MONITORING, <<"voiceMonitoring">>).
-define(TYPE_SET_AGPS, <<"setAgps">>).
-define(TYPE_SET_INDICATOR, <<"setIndicator">>).
-define(TYPE_CONFIGURATION, <<"configuration">>).
-define(TYPE_GET_VERSION, <<"getVersion">>).
-define(TYPE_FIRMWARE_UPDATE, <<"firmwareUpdate">>).
-define(TYPE_SET_CONNECTION, <<"setConnection">>).
-define(TYPE_SET_ODOMETER, <<"setOdometer">>).

-define(TYPE_MODE_POWER_SAVING, <<"modePowerSaving">>).
-define(TYPE_MODE_DEEP_SLEEP, <<"modeDeepSleep">>).

-define(KEY_UNIQUE_ID, <<"uniqueId">>).
-define(KEY_FREQUENCY, <<"frequency">>).
-define(KEY_TIMEZONE, <<"timezone">>).
-define(KEY_DEVICE_PASSWORD, <<"devicePassword">>).
-define(KEY_RADIUS, <<"radius">>).
-define(KEY_MESSAGE, <<"message">>).
-define(KEY_ENABLE, <<"enable">>).
-define(KEY_DATA, <<"data">>).
%%-define(KEY_INDEX, <<"index">>).
-define(KEY_PHONE, <<"phone">>).
-define(KEY_SERVER, <<"server">>).
-define(KEY_PORT, <<"port">>).


-define(ALARM_GENERAL, <<"general">>).
-define(ALARM_SOS, <<"sos">>).
-define(ALARM_VIBRATION, <<"vibration">>).
-define(ALARM_MOVEMENT, <<"movement">>).
-define(ALARM_LOW_SPEED, <<"lowspeed">>).
-define(ALARM_OVERSPEED, <<"overspeed">>).
-define(ALARM_FALL_DOWN, <<"fallDown">>).
-define(ALARM_LOW_POWER, <<"lowPower">>).
-define(ALARM_LOW_BATTERY, <<"lowBattery">>).
-define(ALARM_FAULT, <<"fault">>).
-define(ALARM_POWER_OFF, <<"powerOff">>).
-define(ALARM_POWER_ON, <<"powerOn">>).
-define(ALARM_DOOR, <<"door">>).
-define(ALARM_GEOFENCE, <<"geofence">>).
-define(ALARM_GEOFENCE_ENTER, <<"geofenceEnter">>).
-define(ALARM_GEOFENCE_EXIT, <<"geofenceExit">>).
-define(ALARM_GPS_ANTENNA_CUT, <<"gpsAntennaCut">>).
-define(ALARM_ACCIDENT, <<"accident">>).
-define(ALARM_TOW, <<"tow">>).
-define(ALARM_ACCELERATION, <<"hardAcceleration">>).
-define(ALARM_BREAKING, <<"hardBreaking">>).
-define(ALARM_FATIGUE_DRIVING, <<"fatigueDriving">>).
-define(ALARM_POWER_CUT, <<"powerCut">>).
-define(ALARM_JAMMING, <<"jamming">>).
-define(ALARM_TEMPERATURE, <<"temperature">>).
-define(ALARM_PARKING, <<"parking">>).
-define(ALARM_SHOCK, <<"shock">>).
-define(ALARM_BONNET, <<"bonnet">>).
-define(ALARM_FOOT_BRAKE, <<"footBrake">>).
-define(ALARM_OIL_LEAK, <<"oilLeak">>).
-define(ALARM_TAMPERING, <<"tampering">>).

-endif. % RECORDS_HRL
