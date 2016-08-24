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

-include("em_records.hrl").

%% API
-export([start_link/4]).
-export([init/4]).
-export([test/0]).

-define(Imei, <<"imei:(\\d+),">>).                 % IMEI
-define(Alarm, <<"([^,]+),">>).                     % Alarm
-define(LocalDate, <<"(\\d{2})/?(\\d{2})/?(\\d{2})\\s?">>).                       % Local Date
-define(LocalTime, <<"(\\d{2})(\\d{2}),?">>). % Local Time
-define(Rfid, <<"[^,]*,">>). % rfid
-define(FL, <<"[FL],">>). % full / low
-define(GROUP_BEGIN, <<"(?:">>).
-define(TimeUtc, <<"(\\d{2})(\\d{2})(\\d{2})\\.(\\d+)">>).   % time utc (hhmmss.sss)
-define(OR, <<"|">>).
-define(OFFSET_0, <<"(?:\\d{1,5}\\.\\d+)">>).
-define(GROUP_END, <<")">>).
-define(OFFSET_1, <<",">>).
-define(Validity, <<"([AV]),">>). %% validity
-define(OFFSET_2, <<"(?:([NS]),)?">>).
-define(Latitude, <<"(\\d+)(\\d{2}\\.\\d+),">>).  % Latitude (DDMM.MMMM)
-define(LatitudeType, <<"(?:([NS]),)?">>).
-define(OFFSET_3, <<"(?:([EW])?,)?">>).
-define(Longitude, <<"(\\d+)(\\d{2}\\.\\d+),">>).  % Longitude (DDDMM.MMMM)
-define(LongitudeType, <<"(?:([EW])?,)?">>).
-define(Speed, <<"(\\d+\\.?\\d*)?,?">>).     % Speed
-define(Course, <<"(\\d+\\.?\\d*)?,?">>).           % Course
-define(Altitude, <<"(\\d+\\.?\\d*)?,?">>).        % Altitude
-define(Any, <<".*">>).       % Any

-define(PATTERN,
        <<
          ?Imei/binary,
          ?Alarm/binary,
          ?LocalDate/binary,
          ?LocalTime/binary,
          ?Rfid/binary,
          ?FL/binary,
          ?GROUP_BEGIN/binary,
          ?TimeUtc/binary,
          ?OR/binary,
          ?OFFSET_0/binary,
          ?GROUP_END/binary,
          ?OFFSET_1/binary,
          ?Validity/binary,
          ?OFFSET_2/binary,
          ?Latitude/binary,
          ?LatitudeType/binary,
          ?OFFSET_3/binary,
          ?Longitude/binary,
          ?LongitudeType/binary,
          ?Speed/binary,
          ?Course/binary,
          ?Altitude/binary,
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
            case parse(Data, Pattern) of
                {ok, {Imei, PositionModel}} ->
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
                            em_data_manager:create_position(Object, Position),
                            loop(State#state{device = Object})
                    end;
                {error, Reason} ->
                    em_logger:info("ERROR: ~w", [Reason]),
                    Transport:close(Socket)
            end;
        _ ->
            Transport:close(Socket)
    end.

%% echo "1. gps103"
%% (echo -n -e "imei:123456789012345,help me,1201011201,,F,120100.000,A,6000.0000,N,13000.0000,E,0.00,;";) | nc -v localhost 5001
parse(Data, Pattern) ->
    case data_match(Data, Pattern) of
        {ok, [_, Imei, Alarm, LocalYear, LocalMonth, LocalDay,LocalHour,LocalMinute,UtcHour, UtcMinute, UtcSecond, UtcMillseconds,Validity,LatHem0,LatDD,LatMM_MMMM,LatHem1,LonHem0,LonDDD,LonMM_MMMM,LonHem1,Speed|_]} ->
            Position = #position{
                          deviceTime = parse_date_time(LocalYear, LocalMonth, LocalDay,LocalHour,LocalMinute,UtcHour, UtcMinute, UtcSecond, UtcMillseconds),
                          latitude = parse_coordinate(hen_deg_min_hem, {LatHem0, LatDD, LatMM_MMMM, LatHem1}),
                          longitude = parse_coordinate(hen_deg_min_hem, {LonHem0, LonDDD, LonMM_MMMM, LonHem1}),
                          speed = parse_speed(Speed),
                          course = 0,
                          valid = parse_valid(Validity),
                          attributes = #{
                            ?KEY_ALARM => Alarm
                           }
                         },
            {ok, {Imei, Position}};
        Reason ->
            Reason
    end.

parse_date_time(LocalYear, LocalMonth, LocalDay,LocalHour,LocalMinute,UtcHour, UtcMinute, UtcSecond, _UtcMillseconds) ->
  DeltaMinutes = calc_delta_minutes(
    list_to_integer(binary_to_list(LocalHour)),
    list_to_integer(binary_to_list(LocalMinute)),
    list_to_integer(binary_to_list(UtcHour)),
    list_to_integer(binary_to_list(UtcMinute))
  ),
  Date = {
    {
      list_to_integer(binary_to_list(LocalYear)) + 2000,
      list_to_integer(binary_to_list(LocalMonth)),
      list_to_integer(binary_to_list(LocalDay))
    },
    {
      list_to_integer(binary_to_list(LocalHour)),
      list_to_integer(binary_to_list(LocalMinute)) - DeltaMinutes,
      list_to_integer(binary_to_list(UtcSecond))
    }
  },
  em_helper_time:datetime_to_utc(Date).

parse_coordinate(hen_deg_min_hem, {void, Deg, Min, Hem}) ->
  hemisphere(list_to_integer(binary_to_list(Deg)) + list_to_float(binary_to_list(Min)) / 60, Hem);
parse_coordinate(hen_deg_min_hem, {Hem, Deg, Min, void}) ->
  hemisphere(list_to_integer(binary_to_list(Deg)) + list_to_float(binary_to_list(Min)) / 60, Hem).

hemisphere(Coordinate, <<"S">>) -> -1 * Coordinate;
hemisphere(Coordinate, <<"W">>) -> -1 * Coordinate;
hemisphere(Coordinate, <<"-">>) -> -1 * Coordinate;
hemisphere(Coordinate, _) -> Coordinate.


calc_delta_minutes(LocalHours, LocalMinutes, UtcHours, UtcMinutes) ->
    delta((LocalHours - UtcHours) * 60 + LocalMinutes - UtcMinutes).

delta(DeltaMinutes) when (DeltaMinutes =< -12 * 60) ->
    DeltaMinutes + 24 * 60;
delta(DeltaMinutes) when (DeltaMinutes > 12 * 60) ->
    DeltaMinutes - 24 * 60;
delta(DeltaMinutes) ->
    DeltaMinutes.

parse_speed(Speed) ->
    list_to_float(binary_to_list(Speed)).

parse_valid(Validity) when is_binary(Validity) ->
    binary_to_atom(Validity, utf8).


data_match(Data, Pattern) ->
  case re:run(Data, Pattern) of
    {match, List} ->
      Res =lists:reverse(lists:foldl(fun(Param, Res) ->
        [read_param(Param, Data) | Res]
                                     end, [], List)),
      {ok, Res};
    Reason ->
      {error, Reason}
  end.

read_param({-1, 0}, _) ->
  void;
read_param({Pos, Len}, Data) ->
  binary:part(Data, Pos, Len).

test() ->
    Packet = <<"imei:123456789012345,help me,1201011201,,F,120100.000,A,6000.0000,N,13000.0000,E,0.00,;">>,
    {ok, Pattern} = re:compile(?PATTERN),
    parse(Packet, Pattern).
