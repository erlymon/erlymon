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
-module(em_gl100_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(ranch_protocol).

-include("em_records.hrl").

%% API
-export([start_link/4]).
-export([init/4]).
-export([test/0]).

-define(Command, <<"\\+RESP:GT...,">>).
-define(Imei, <<"(\\d{15}),">>). %% Imei
-define(Number, <<"(?:(?:\\d+,">>).%% Number
-define(GeofenceId, <<"\\d,">>). %% Reserved / Geofence id
-define(GeofenceAlert, <<"\\d)|">>). %%  Reserved / Geofence alert
-define(CallingNumber, <<"(?:[^,]*)),">>). %% Calling number
-define(GpsFix, <<"([01]),">>). %% GPS fix
-define(Speed, <<"(\\d+.\\d),">>). %% Speed
-define(Course, <<"(\\d+),">>).%% course
-define(Altitude, <<"(-?\\d+.\\d),">>). %% altitude
-define(GpsAccuracy, <<"\\d*,">>). %% gps accuracy
-define(Longitude, <<"(-?\\d+.\\d+),">>). %% longitude
-define(Latitude, <<"(-?\\d+.\\d+),">>). %% latitude
-define(Date, <<"(\\d{4})(\\d{2})(\\d{2})">>). %% date (YYYYMMDD)
-define(Time, <<"(\\d{2})(\\d{2})(\\d{2}),">>). % time (HHMMSS)
-define(Any, <<".*">>).

-define(PATTERN,
  <<
    ?Command/binary,
    ?Imei/binary,
    ?Number/binary,
    ?GeofenceId/binary,
    ?GeofenceAlert/binary,
    ?CallingNumber/binary,
    ?GpsFix/binary,
    ?Speed/binary,
    ?Course/binary,
    ?Altitude/binary,
    ?GpsAccuracy/binary,
    ?Longitude/binary,
    ?Latitude/binary,
    ?Date/binary,
    ?Time/binary,
    ?Any/binary
  >>
).

-record(state, {protocol, transport, socket, timeout, device, pattern}).

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
  ok = ranch:accept_ack(Ref),
  Protocol = proplists:get_value(protocol, Opts),
  {ok, Pattern} = re:compile(?PATTERN),
  loop(#state{protocol = Protocol, socket = Socket, transport = Transport, pattern = Pattern}).


loop(State = #state{protocol = Protocol, socket = Socket, transport = Transport, pattern = Pattern}) ->
  case Transport:recv(Socket, 0, 5000) of
    {ok, Data} ->
      em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_hardware:resolve(Socket), Data]),
      {Imei, PositionModel} = parse(Data, Pattern),
      case em_data_manager:get_device_by_uid(Imei) of
        {error, _Reason} ->
          em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_hardware:resolve(Socket), Imei]),
          Transport:close(Socket);
        {ok, Object} ->
          Position = PositionModel#position{
            deviceId = Object#device.id,
            protocol = atom_to_binary(Protocol, utf8),
            attributes = #{
              ?KEY_IP => em_hardware:resolve(Socket)
            }
          },
          em_logger:info("save message => unit: ip = '~s' id = '~w' imei = '~s' position: ~w", [em_hardware:resolve(Socket), Object#device.id, Imei, Position]),
          em_data_manager:create_position(Object, Position),
          loop(State#state{device = Object})
      end;
    _ ->
      Transport:close(Socket)
  end.

% echo "3. gl100"
% (echo -n -e "+RESP:GTSOS,123456789012345,0,0,0,1,0.0,0,0.0,1,130.000000,60.000000,20120101120300,0460,0000,18d8,6141,00,11F0,0102120204\0";) | nc -v localhost 5003
parse(Data, Pattern) -> 
    case data_match(Data, Pattern) of
        [_, Imei, Validity, Speed, Course, Altitude, Longitude, Latitude, Year, Month, Day, Hour, Minute, Second | _] ->
            Position = #position{
              deviceTime = parse_date(Year, Month, Day, Hour, Minute, Second),
              latitude = parse_coord(Latitude),
              longitude = parse_coord(Longitude),
              speed = parse_speed(Speed),
              course = parse_course(Course),
              altitude = parse_altitude(Altitude),
              valid = parse_valid(Validity)
            },
            {Imei, Position};
        _ ->
            {}
    end.


parse_altitude(Altitude) ->
    list_to_float(binary_to_list(Altitude)).

parse_speed(Speed) ->
     list_to_float(binary_to_list(Speed)).

parse_course(Course) ->
    list_to_integer(binary_to_list(Course)).

parse_coord(Coord) ->
    list_to_float(binary_to_list(Coord)).

parse_valid(Validity) when Validity == <<"1">> ->
    true;
parse_valid(Validity) when Validity == <<"0">> ->
    false.



%parse_device_id(DeviceId) ->
%    list_to_integer(binary_to_list(DeviceId)).

parse_date(Year, Month, Day, Hour, Minute, Second) ->
    Date = {
      {
        list_to_integer(binary_to_list(Year)),
        list_to_integer(binary_to_list(Month)), 
        list_to_integer(binary_to_list(Day))
      },
      {
        list_to_integer(binary_to_list(Hour)), 
        list_to_integer(binary_to_list(Minute)),
        list_to_integer(binary_to_list(Second)) 
      }
     },
    em_logger:info("DATE: ~w", [Date]),
    em_helper_time:datetime_to_utc(Date).

data_match(Data, Pattern) ->
  {match, List} = re:run(Data, Pattern),
  lists:reverse(lists:foldl(fun(Param, Res) ->
                                    [read_param(Param, Data) | Res]
                            end, [], List)).

read_param({-1, 0}, _) ->
    void;
read_param({Pos, Len}, Data) ->
    binary:part(Data, Pos, Len).

test() ->
    Packet = <<"+RESP:GTSOS,123456789012345,0,0,0,1,0.0,0,0.0,1,130.000000,60.000000,20120101120300,0460,0000,18d8,6141,00,11F0,0102120204\0">>,
    {ok, Pattern} = re:compile(?PATTERN),
    parse(Packet, Pattern).
