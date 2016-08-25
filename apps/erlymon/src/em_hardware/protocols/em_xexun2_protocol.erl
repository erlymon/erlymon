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
-module(em_xexun2_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(ranch_protocol).

-include("em_records.hrl").

%% API
-export([start_link/4]).
-export([init/4]).
-export([test/0]).

-define(Header0, <<"[\r\n]*">>).           %%
-define(Serial, <<"(\\d+),">>).           %%  Serial
-define(Number, <<"([^,]+)?,">>).           %%  Number
-define(Header1, <<"G[PN]RMC,">>).           %%
-define(Time, <<"(\\d{2})(\\d{2})(\\d{2})\\.(\\d+),">>).           %% Time (HHMMSS.SSS)
-define(Validity, <<"([AV]),">>).           %% Validity
-define(Latitude, <<"(\\d{2})(\\d{2}\\.\\d+),">>).           %% Latitude (DDMM.MMMM)
-define(LatitudeType, <<"([NS]),">>).           %%
-define(Longitude, <<"(\\d{3})(\\d{2}\\.\\d+),">>).           %% Longitude (DDDMM.MMMM)
-define(LongitudeType, <<"([EW]),">>).           %%
-define(Speed, <<"(\\d+\\.\\d+),">>).           %% Speed
-define(Course, <<"(\\d+\\.\\d+)?,">>).           %% Course
-define(Date, <<"(\\d{2})(\\d{2})(\\d{2}),">>).           %% Date (DDMMYY)
-define(Checksum, <<"[^\\*]*\\*..,">>).           %% Checksum
-define(Signal, <<"([FL]),">>).           %%  Signal
-define(Alarm, <<"(?:([^,]*),)?">>).           %% Alarm
-define(ImeiOffset, <<".*imei:">>).           %%
-define(Imei, <<"(\\d+),">>).           %% IMEI
-define(Satellites, <<"(\\d+),">>).           %% Satellites
-define(Altitude, <<"(-?\\d+\\.\\d+)?,">>).           %% Altitude
-define(Power, <<"[FL]:(\\d+\\.\\d+)V">>).           %% Power
-define(Any, <<".*">>).
-define(Tail, <<"[\r\n]*">>).

-define(PATTERN,
        <<
          ?Header0/binary,
          ?Serial/binary,
          ?Number/binary,
          ?Header1/binary,
          ?Time/binary,
          ?Validity/binary,
          ?Latitude/binary,
          ?LatitudeType/binary,
          ?Longitude/binary,
          ?LongitudeType/binary,
          ?Speed/binary,
          ?Course/binary,
          ?Date/binary,
          ?Checksum/binary,
          ?Signal/binary,
          ?Alarm/binary,
          ?ImeiOffset/binary,
          ?Imei/binary,
          ?Satellites/binary,
          ?Altitude/binary,
          ?Power/binary,
          ?Any/binary,
          ?Tail/binary
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

loop(State = #state{protocol = Protocol, socket = Socket, transport = Transport}) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
            em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_hardware:resolve(Socket), Data]),
            {Imei, PositionModel} = parse(Data),
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
        _ ->
            Transport:close(Socket)
    end.

%% echo "6. xexun2"
%% (echo -n -e "111111120009,+436763737552,GPRMC,120600.000,A,6000.0000,N,13000.0000,E,0.00,0.00,010112,,,A*68,F,, imei:123456789012345,04,481.2,F:4.15V,0,139,2689,232,03,2725,0576\n";) | nc -v localhost 5006
parse(Data) ->
    case data_match(Data, ?PATTERN) of
        [_, _Serial, _PhoneNumber, Hour, Minute, Second, _Millisecond, Validity, LatDD, LatMM_MMMM, LatType, LonDD, LonMM_MMMM, LonType, Speed, Course, Day, Month, Year, _Signal, Alarm, Imei, Sattelite, Altitude, Power| _] ->
            Position = #position{
                          deviceTime = parse_date(Year, Month, Day, Hour, Minute, Second),
                          latitude = parse_coord(LatDD, LatMM_MMMM, LatType),
                          longitude = parse_coord(LonDD, LonMM_MMMM, LonType),
                          altitude = parse_altitude(Altitude),
                          speed = parse_speed(Speed),
                          course = parse_course(Course),
                          valid = parse_valid(Validity),
                          attributes = #{
                            ?KEY_POWER => parse_power(Power),
                            ?KEY_ALARM => Alarm,
                            ?KEY_SATELLITES => parse_sattelite(Sattelite)
                           }
                         },
            {Imei, Position};
        _ ->
            {}
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

parse_sattelite(Satteliate) ->
    list_to_integer(binary_to_list(Satteliate)).

parse_power(Power) ->
    list_to_float(binary_to_list(Power)).

parse_altitude(Altitude) ->
    list_to_float(binary_to_list(Altitude)).

parse_speed(Speed) ->
    list_to_float(binary_to_list(Speed)).

parse_course(Course) ->
    list_to_float(binary_to_list(Course)).

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
    Packet = <<"111111120009,+436763737552,GPRMC,120600.000,A,6000.0000,N,13000.0000,E,0.00,0.00,010112,,,A*68,F,, imei:123456789012345,04,481.2,F:4.15V,0,139,2689,232,03,2725,0576\n">>,
    parse(Packet).
