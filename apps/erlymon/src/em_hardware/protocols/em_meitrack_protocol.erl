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
-module(em_meitrack_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(ranch_protocol).
-include("em_records.hrl").

%% API
-export([start_link/4]).
-export([init/4]).
-export([test/0]).


-define(PATTERN, list_to_binary([
    "\\$\\$." ++
    "\\d+," ++
    "(\\d+)," ++
    "[0-9a-fA-F]{3}," ++
    "(?:\\d+,)?(\\d+)," ++
    "(-?\\d+\\.\\d+)," ++
    "(-?\\d+\\.\\d+)," ++
    "(\\d{2})(\\d{2})(\\d{2})" ++
    "(\\d{2})(\\d{2})(\\d{2})," ++
    "([AV])," ++
    "(\\d+)," ++
    "(\\d+)," ++
    "(\\d+\\.?\\d*)," ++
    "(\\d+)," ++
    "(\\d+\\.?\\d*)," ++
    "(-?\\d+)," ++
    "(\\d+)," ++
    "(\\d+)," ++
    "(\\d+)\\|" ++
    "(\\d+)\\|" ++
    "([0-9a-fA-F]+)\\|" ++
    "([0-9a-fA-F]+)," ++
    "([0-9a-fA-F]+)," ++
    "([0-9a-fA-F]+)?\\|" ++
    "([0-9a-fA-F]+)?\\|" ++
    "([0-9a-fA-F]+)?\\|" ++
    "([0-9a-fA-F]+)\\|" ++
    "([0-9a-fA-F]+)," ++
    ".*?"
])).

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
      case parse(Data) of
        {ok, Imei, PositionModel} ->
          em_logger:info("[packet] unit: ip = '~s' imei = '~s' message: ~w", [em_hardware:resolve(Socket), Imei, PositionModel]),
          case em_data_manager:get_device_by_uid(Imei) of
            {error, _Reason} ->
              em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_hardware:resolve(Socket), Imei]),
              Transport:close(Socket);
            {ok, Object} ->
              Position = PositionModel#position{
                deviceId = Object#device.id,
                protocol = atom_to_binary(Protocol, utf8),
                attributes = maps:merge(PositionModel#position.attributes, #{
                  ?KEY_IP => em_hardware:resolve(Socket)
                })
              },
              em_logger:info("save message => unit: ip = '~s' id = '~w' imei = '~s' position: ~w", [em_hardware:resolve(Socket), Object#device.id, Imei, Position]),
              em_data_manager:create_position(Object#device.id, Position),
              loop(State#state{device = Object})
          end;
        {error, Message} ->
          em_logger:info("ERROR: ~s", [Message]),
          Transport:close(Socket)
      end;
    _ ->
      Transport:close(Socket)
  end.

%% echo "20. meitrack"
%% (echo -n -e "\$\$d138,123456789012345,AAA,35,60.000000,130.000000,120101122000,A,7,18,0,0,0,49,3800,24965,510|10|0081|4F4F,0000,000D|0010|0012|0963|0000,,*BF\r\n";) | nc -v localhost 5020
parse(Data) ->
  case em_regexp:match(Data, ?PATTERN) of
    {ok, [_, Imei, _Event, Latitude, Longitude, Year, Month, Day, Hour, Minute, Second, Validity, Satellites, _GsmSignal, Speed, Course, _Hdop, Altitude, _Odometer, _Runtime, _Cell, _State, _Adc1, _Adc2, _Adc3, _Battery, Power | _]} ->
      Position = #position{
        deviceTime = parse_date(Year, Month, Day, Hour, Minute, Second),
        latitude = list_to_float(binary_to_list(Latitude)),
        longitude = list_to_float(binary_to_list(Longitude)),
        altitude = parse_altitude(Altitude),
        speed = parse_speed(Speed),
        course = parse_course(Course),
        valid = parse_valid(Validity),
        attributes = #{
          ?KEY_SATELLITES => parse_satellites(Satellites),
          ?KEY_POWER => parse_power(Power)
        }
      },
      {ok, Imei, Position};
    Reason ->
      Reason
  end.

parse_satellites(Satteliates) ->
  list_to_integer(binary_to_list(Satteliates)).

parse_power(Power) ->
  em_logger:info("PARSER POWER: '~w'", [Power]),
  erlang:binary_to_integer(Power, 16).
%%list_to_integer(binary_to_list(Power)).

parse_altitude(Altitude) ->
  list_to_integer(binary_to_list(Altitude)).

parse_speed(Speed) ->
  list_to_integer(binary_to_list(Speed)).

parse_course(Course) ->
  list_to_integer(binary_to_list(Course)).

parse_valid(Validity) when Validity == <<"A">> ->
  true;
parse_valid(Validity) when Validity == <<"V">> ->
  false.


%parse_device_id(DeviceId) ->
%    list_to_integer(binary_to_list(DeviceId)).

parse_date(Year, Month, Day, Hour, Minute, Second) ->
  Date = {
    {
      list_to_integer(binary_to_list(Year)) + 2000,
      list_to_integer(binary_to_list(Month)),
      list_to_integer(binary_to_list(Day))
    },
    {
      list_to_integer(binary_to_list(Hour)),
      list_to_integer(binary_to_list(Minute)),
      list_to_integer(binary_to_list(Second))
    }
  },
  em_helper_time:datetime_to_utc(Date).

%% $$J139,359231038158125,AAA,35,53.897721,27.443013,161125211605,A,5,30,0,4,3.4,252,1070708,1506398,257|4|0000|0000,0000,0007|0007||02DD|00FE,*E8
%% $$K139,359231038158125,AAA,35,53.897721,27.443013,161125211605,A,5,30,0,4,3.4,252,1070708,1506398,257|4|0000|0000,0000,0007|0007||02DD|00FE,*E9
test() ->
  %%Packet = <<"\$\$d138,123456789012345,AAA,35,60.000000,130.000000,120101122000,A,7,18,0,0,0,49,3800,24965,510|10|0081|4F4F,0000,000D|0010|0012|0963|0000,,*BF\r\n">>,
  Packet = <<"\$\$K139,359231038158125,AAA,35,53.897721,27.443013,161126081580,A,5,30,0,4,3.4,252,1070708,1506398,257|4|0000|0000,0000,0007|0007||02DD|00FE,*E9\r\n">>,
  em_regexp:match(Packet, ?PATTERN).
