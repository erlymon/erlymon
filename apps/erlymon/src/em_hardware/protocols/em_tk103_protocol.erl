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
-module(em_tk103_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(ranch_protocol).

%% API
-export([start_link/4]).
-export([init/4]).
-export([test/0]).

%% a = "(123456789012BP05123456789012345120101A6000.0000N13000.0000E000.0120200000.0000000000L000946BB)"
%% pattern = "(\\d+)(?:,?)(.{4}),?(\\d*)(\\d{2})(\\d{2})(\\d{2}),?([AV]),?(\\d{2})(\\d{2}\\.\\d+)([NS]),?(\\d{3})(\\d{2}\\.\\d+)([EW]),?(\\d+\\.\\d)(?:\\d*,)?(\\d{2})(\\d{2})(\\\d{2}),?(\\d{3}\\.?\\d{2}),?([0-9a-fA-F]{8})?,?(?:L([0-9a-fA-F]+))?\\)?"
%% a.match(pattern);

%%  pattern = "(\\d+)(?:,?)(.{4}),?(\\d*)(\\d{2})(\\d{2})(\\d{2}),?([AV]),?(\\d{2})(\\d{2}\\.\\d+)([NS]),?(\\d{3})(\\d{2}\\.\\d+)([EW]),?(\\d+\\.\\d)(?:\\d*,)?(\\d{2})(\\d{2})(\\d{2}),?(\\d{3}\\.?\\d{2}),?([0-9a-fA-F]{8})?,?(?:L([0-9a-fA-F]+))?\\)?"

-define(DeviceID, <<"(\\d+),?">>).                 % Device ID
-define(Command, <<"(.{4}),?">>).                     % Command
-define(IMEI, <<"(\\d*)">>).                       % IMEI (?)
-define(Date, <<"(\\d{2})(\\d{2})(\\d{2}),?">>). % Date (YYMMDD)
-define(Validity, <<"([AV]),?">>).                   % Validity
-define(Latitude, <<"(\\d{2})(\\d{2}\\.\\d+)">>).  % Latitude (DDMM.MMMM)
-define(LatitudeType, <<"([NS]),?">>).
-define(Longitude, <<"(\\d{3})(\\d{2}\\.\\d+)">>).  % Longitude (DDDMM.MMMM)
-define(LongitudeType, <<"([EW]),?">>).
-define(Speed, <<"(\\d+\\.\\d)(?:\\d*,)?">>).     % Speed
-define(Time, <<"(\\d{2})(\\d{2})(\\d{2}),?">>). % Time (HHMMSS)
-define(Course, <<"(\\d{3}\\.?\\d{2}),?">>).           % Course
-define(State, <<"([0-9a-fA-F]{8})?,?">>).        % State
-define(Millage, <<"(?:L([0-9a-fA-F]+))?">>).       % Millage

-define(PATTERN,
  <<
  ?DeviceID/binary,
  ?Command/binary,
  ?IMEI/binary,
  ?Date/binary,
  ?Validity/binary,
  ?Latitude/binary,
  ?LatitudeType/binary,
  ?Longitude/binary,
  ?LongitudeType/binary,
  ?Speed/binary,
  ?Time/binary,
  ?Course/binary,
  ?State/binary,
  ?Millage/binary
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

% echo "2. tk103"
% (echo -n -e "(123456789012BP05123456789012345120101A6000.0000N13000.0000E000.0120200000.0000000000L000946BB)";) | nc -v localhost 5002
parse(Data, Pattern) ->
    case data_match(Data, Pattern) of
        [_, DeviceId, Command, Imei, Year, Month, Day, Validity, LatDD, LatMM_MMMM, LatType, LonDD, LonMM_MMMM, LonType, Speed, Hour, Minute, Second, Course, State, Millage] ->
            Message = #{
              <<"deviceTime">> => parse_date(Year, Month, Day, Hour, Minute, Second),
              <<"latitude">> => parse_coord(LatDD, LatMM_MMMM, LatType),
              <<"longitude">> => parse_coord(LonDD, LonMM_MMMM, LonType),
              <<"speed">> => parse_speed(Speed),
              <<"course">> => parse_course(Course),
              <<"deviceId">> => parse_device_id(DeviceId),
              <<"valid">> => parse_validity(Validity)
             },
            {Imei, Message};
        _ ->
            #{}
    end.

parse_coord(CoordDD, CoordMM_MMMM, CoordType) ->
    Coord = list_to_integer(binary_to_list(CoordDD)) + list_to_float(binary_to_list(CoordMM_MMMM)) / 60,
    case CoordType of
        <<"S">> ->
            Coord * -1;
        <<"N">> ->
            Coord;
        <<"W">> ->
            Coord * -1;
        <<"E">> ->
            Coord
    end.

parse_course(Course) ->
    list_to_float(binary_to_list(Course)).
parse_speed(Speed) ->
    list_to_float(binary_to_list(Speed)).

parse_device_id(DeviceId) ->
    list_to_integer(binary_to_list(DeviceId)).

parse_validity(<<"A">>) -> true;
parse_validity(_) -> false.

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

%% (123456789012 BP05 123456789012345 120101 A 6000.0000N 13000.0000E 000.0 120200 000.00 00000000 L000946BB)
test() ->
  Packet = <<"(123456789012BP05123456789012345120101A6000.0000N13000.0000E000.0120200000.0000000000L000946BB)">>,
  {ok, Pattern} = re:compile(?PATTERN),
  parse(Packet, Pattern).
