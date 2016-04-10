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
-module(em_gps103_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(ranch_protocol).

%% API
-export([start_link/4]).
-export([init/4]).
-export([test/0]).

-record(state, {protocol, transport, socket, timeout, device}).

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.


init(Ref, Socket, Transport, Opts) ->
  ok = ranch:accept_ack(Ref),
  Protocol = proplists:get_value(protocol, Opts),
  loop(#state{protocol = Protocol, socket = Socket, transport = Transport}).


loop(State = #state{protocol = Protocol, socket = Socket, transport = Transport}) ->
  case Transport:recv(Socket, 0, 5000) of
    {ok, Data} ->
      em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_hardware:resolve(Socket), Data]),
      {Imei, Message} = parse(Data),
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

% echo "1. gps103"
% (echo -n -e "imei:123456789012345,help me,1201011201,,F,120100.000,A,6000.0000,N,13000.0000,E,0.00,;";) | nc -v localhost 5001
parse(Data) when is_binary(Data) ->
  [Imei, Alarm, LocalDateTime, _, _Status, TimeUtc, Validity, Latitude, LatitudeType, Longitude, LongitudeType, Speed, _Course| _] = binary:split(Data, [<<",">>], [global]),
  UtcTime = parse_date_time(LocalDateTime, TimeUtc),
  {LongitudeDegrees, LatitudeDegrees} = parse_coordinates({{Longitude, LongitudeType}, {Latitude, LatitudeType}}),
  Message = #{
    <<"deviceTime">> => UtcTime,
    <<"latitude">> => LatitudeDegrees,
    <<"longitude">> => LongitudeDegrees,
    <<"speed">> => parse_speed(Speed),
    <<"course">> => 0,
    <<"valid">> =>parse_validity(Validity),
    <<"alarm">> => Alarm
  },
  {parse_imei(Imei), Message}.

parse_imei(ImeiData) when is_binary(ImeiData) ->
  [_, Imei|_] = binary:split(ImeiData, [<<":">>], [global]),
  Imei.

parse_date_time(LocalDateTime, TimeUtc) when is_binary(LocalDateTime) ->
  {ok, [Years, Month, Days, LocalHours, LocalMinutes], []} = io_lib:fread("~2s~2s~2s~2s~2s", binary_to_list(LocalDateTime)),
  {ok, [UtcHours, UtcMinutes, UtcSeconds, _UtcMillseconds], []} = io_lib:fread("~2s~2s~2s.~3s", binary_to_list(TimeUtc)),
  DeltaMinutes = calc_delta_minutes(list_to_integer(LocalHours), list_to_integer(LocalMinutes), list_to_integer(UtcHours), list_to_integer(UtcMinutes)),
  BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  (calendar:datetime_to_gregorian_seconds({{list_to_integer(Years) + 2000, list_to_integer(Month), list_to_integer(Days)}, {list_to_integer(LocalHours), list_to_integer(LocalMinutes) - DeltaMinutes, list_to_integer(UtcSeconds)}}) - BaseDate) * 1000.

calc_delta_minutes(LocalHours, LocalMinutes, UtcHours, UtcMinutes) ->
  delta((LocalHours - UtcHours) * 60 + LocalMinutes - UtcMinutes).

delta(DeltaMinutes) when (DeltaMinutes =< -12 * 60) ->
  DeltaMinutes + 24 * 60;
delta(DeltaMinutes) when (DeltaMinutes > 12 * 60) ->
  DeltaMinutes - 24 * 60;
delta(DeltaMinutes) ->
  DeltaMinutes.

parse_coordinates({{Longitude, LongitudeType}, {Latitude, LatitudeType}}) ->
  em_hardware:to_wgs84({
    {
      list_to_float(binary_to_list(Longitude)),
      binary_to_atom(LongitudeType, utf8)
    },
    {
      list_to_float(binary_to_list(Latitude)),
      binary_to_atom(LatitudeType, utf8)
    }
  }).

parse_speed(Speed) ->
  list_to_float(binary_to_list(Speed)).

parse_course(Course) ->
  list_to_float(binary_to_list(Course)).

parse_validity(Validity) when is_binary(Validity) ->
  binary_to_atom(Validity, utf8).

test() ->
  Packet = <<"imei:123456789012345,help me,1201011201,,F,120100.000,A,6000.0000,N,13000.0000,E,0.00,;">>,
  parse(Packet).
