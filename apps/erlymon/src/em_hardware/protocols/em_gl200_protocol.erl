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
-module(em_gl200_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(ranch_protocol).

%% API
-export([start_link/4]).
-export([init/4]).
-export([test/0]).

-define(Header0, <<"(?:(?:\\+(?:RESP|BUFF):)|">>).
-define(Header1, <<"(?:\\x00?\\x04,[A-Fa-f0-9]{4},[01],))">>).
-define(Header2, <<"GT...,">>).
-define(ProtocolVersion, <<"(?:[0-9a-fA-F]{6})?,">>). %% Protocol version
-define(Imei, <<"(\\d{15}),.*,">>). %% IMEI
-define(GpsAccuracy, <<"(\\d*),">>). %% GPS accuracy
-define(Speed, <<"(\\d+.\\d)?,">>). %% Speed
-define(Course, <<"(\\d+)?,">>).%% Course
-define(Altitude, <<"(-?\\d+\\.\\d)?,">>). %% Altitude
-define(Longitude, <<"(-?\\d+\\.\\d+),">>). %% Longitude
-define(Latitude, <<"(-?\\d+\\.\\d+),">>).%% Latitude
-define(Date, <<"(\\d{4})(\\d{2})(\\d{2})">>). %% Date (YYYYMMDD)
-define(Time, <<"(\\d{2})(\\d{2})(\\d{2}),">>). %% Time (HHMMSS)
-define(MCC, <<"(\\d{4})?,">>). %% MCC
-define(MNC, <<"(\\d{4})?,">>). %% MNC
-define(LAC, <<"([A-Fa-f0-9]{4})?,">>). %% LAC
-define(Cell, <<"([A-Fa-f0-9]{4})?,">>). %% Cell
-define(Odometer, <<"(?:(\\d+\\.\\d)?,">>). %% Odometer
-define(Battery, <<"(\\d{1,3})?)?">>). %% Battery
-define(Any, <<".*">>).

-define(PATTERN,
  <<
    ?Header0/binary,
    ?Header1/binary,
    ?Header2/binary,
    ?ProtocolVersion/binary,
    ?Imei/binary,
    ?GpsAccuracy/binary,
    ?Speed/binary,
    ?Course/binary,
    ?Altitude/binary,
    ?Longitude/binary,
    ?Latitude/binary,
    ?Date/binary,
    ?Time/binary,
    ?MCC/binary,
    ?MNC/binary,
    ?LAC/binary,
    ?Cell/binary,
    ?Odometer/binary,
    ?Battery/binary,
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
      {Imei, Message} = parse(Data, Pattern),
      case em_data_manager:get_device_by_uid(Imei) of
        null ->
          em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_hardware:resolve(Socket), Imei]),
          Transport:close(Socket);
        Object ->
          em_logger:info("save message => unit: ip = '~s' id = '~w' imei = '~s' message: ~s", [em_hardware:resolve(Socket), maps:get(<<"id">>, Object), Imei, em_json:encode(Message)]),
          em_data_manager:create_message(maps:get(<<"id">>, Object), Protocol, maps:merge(#{imei => maps:get(<<"uniqueId">>, Object)}, Message)),
          loop(State#state{device = Object})
      end;
    _ ->
      Transport:close(Socket)
  end.

% echo "4. gl200"
% (echo -n -e "+RESP:GTFRI,020102,123456789012345,,0,0,1,1,0.0,0,0.0,130.000000,60.000000,20120101120400,0460,0000,18d8,6141,00,,20120101120400,11F0\$";) | nc -v localhost 5004
parse(Data, Pattern) -> 
    case data_match(Data, Pattern) of
        [_, Imei, Validity, Speed, Course, Altitude, Longitude, Latitude, Year, Month, Day, Hour, Minute, Second,  Mcc, Mnc, Lac, Cell | _] ->
            Message = #{
              <<"deviceTime">> => parse_date(Year, Month, Day, Hour, Minute, Second),
              <<"latitude">> => parse_coord(Latitude),
              <<"longitude">> => parse_coord(Longitude),
              <<"altitude">> => parse_altitude(Altitude),
              <<"speed">> => parse_speed(Speed),
              <<"course">> => parse_course(Course),
              <<"valid">> => parse_valid(Validity),
              <<"mcc">> => parse_mcc(Mcc),
              <<"mnc">> => parse_mnc(Mnc),
              <<"lac">> => parse_lac(Lac),
              <<"cell">> => parse_cell(Cell)
              %%<<"odometer">> => parse_odometer(Odometer),
              %%<<"battery">> => parse_battery(Battery)
             },
            {Imei, Message};
        _ ->
            {}
    end.

parse_mcc(Mcc) ->
    Mcc.

parse_mnc(Mnc) ->
    Mnc.

parse_lac(Lac) ->
    Lac.

parse_cell(Cell) ->
    Cell.

parse_odometer(Odometer) ->
    Odometer.

parse_battery(Battery) ->
    Battery.

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
    em_hardware:to_timestamp(Date).

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
    Packet = <<"+RESP:GTFRI,020102,123456789012345,,0,0,1,1,0.0,0,0.0,130.000000,60.000000,20120101120400,0460,0000,18d8,6141,00,,20120101120400,11F0\$">>,
    {ok, Pattern} = re:compile(?PATTERN),
    parse(Packet, Pattern).
