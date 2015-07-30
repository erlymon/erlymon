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
-module(em_t55_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(ranch_protocol).

%% API
-export([start_link/4]).
-export([init/4]).
-export([test/0]).

-define(PATTERN, <<"(?:\\$?)([A-Z]*),(.*)">>).

-define(PGID_PATTERN, <<"\\$PGID,(\\d*)\\*([0-9,a-f,A-F]{2})">>).
-define(PCPTI_PATTERN, <<"\\$PCPTI,(\\d*),.*">>).
-define(GPFID_PATTERN, <<"\\$GPFID(\\d*)">>).
-define(IMEI_PATTERN, <<"IMEI(\\d*)">>).

-define(GPRMC_HEADER, <<"\\$GPRMC,">>).
-define(GPRMC_TIME, <<"(\\d{2})(\\d{2})(\\d{2})\\.?\\d*,">>).   %% Time (HHMMSS.SSS)
-define(GPRMC_VALIDITY, <<"([AV]),">>).   %% Validity
-define(GPRMC_LATITUDE, <<"(\\d{2})(\\d{2}\\.\\d+),">>).   %% Latitude (DDMM.MMMM)
-define(GPRMC_LATITUDE_TYPE, <<"([NS]),">>).   %%
-define(GPRMC_LONGITUDE, <<"(\\d{3})(\\d{2}\\.\\d+),">>).   %% Longitude (DDDMM.MMMM)
-define(GPRMC_LONGITUDE_TYPE, <<"([EW]),">>).   %%
-define(GPRMC_SPEED, <<"(\\d+\\.?\\d*)?,">>).   %% Speed
-define(GPRMC_COURSE, <<"(\\d+\\.?\\d*)?,">>).   %% Course
-define(GPRMC_DATE, <<"(\\d{2})(\\d{2})(\\d{2})">>).   %% Date (DDMMYY)
-define(GPRMC_ANY, <<".+">>).

-define(GPRMC_PATTERN,
  <<
  ?GPRMC_HEADER/binary,
  ?GPRMC_TIME/binary,
  ?GPRMC_VALIDITY/binary,
  ?GPRMC_LATITUDE/binary,
  ?GPRMC_LATITUDE_TYPE/binary,
  ?GPRMC_LONGITUDE/binary,
  ?GPRMC_LONGITUDE_TYPE/binary,
  ?GPRMC_SPEED/binary,
  ?GPRMC_COURSE/binary,
  ?GPRMC_DATE/binary,
  ?GPRMC_ANY/binary
  >>
).

-define(GPGGA_HEADER, <<"\\$GPGGA,">>).   %%
-define(GPGGA_TIME, <<"(\\d{2})(\\d{2})(\\d{2})\\.?\\d*,">>).   %% Time
-define(GPGGA_LATITUDE, <<"(\\d{2})(\\d{2}\\.\\d+),">>).   %% Latitude
-define(GPGGA_LATITUDE_TYPE, <<"([NS]),">>).   %%
-define(GPGGA_LONGITUDE, <<"(\\d{3})(\\d{2}\\.\\d+),">>).   %% Longitude
-define(GPGGA_LONGITUDE_TYPE, <<"([EW]),">>).   %%
-define(GPGGA_ANY, <<".+">>).   %%

-define(GPGGA_PATTERN,
  <<
  ?GPGGA_HEADER/binary,
  ?GPGGA_TIME/binary,
  ?GPGGA_LATITUDE/binary,
  ?GPGGA_LATITUDE_TYPE/binary,
  ?GPGGA_LONGITUDE/binary,
  ?GPGGA_LONGITUDE_TYPE/binary,
  ?GPGGA_ANY/binary
  >>
).

-define(GPRMA_HEADER, <<"\\$GPRMA,">>). %%
-define(GPRMA_VALIDITY, <<"([AV]),">>). %% Validity
-define(GPRMA_LATITUDE, <<"(\\d{2})(\\d{2}\\.\\d+),">>). %% Latitude
-define(GPRMA_LATITUDE_TYPE, <<"([NS]),">>). %%
-define(GPRMA_LONGITUDE, <<"(\\d{3})(\\d{2}\\.\\d+),">>). %% Longitude
-define(GPRMA_LONGITUDE_TYPE, <<"([EW]),,,">>). %%
-define(GPRMA_SPEED, <<"(\\d+\\.?\\d*)?,">>). %% Speed
-define(GPRMA_COURSE, <<"(\\d+\\.?\\d*)?,">>). %% Course
-define(GPRMA_ANY, <<".+">>). %%

-define(GPRMA_PATTERN,
  <<
  ?GPRMA_HEADER/binary,
  ?GPRMA_VALIDITY/binary,
  ?GPRMA_LATITUDE/binary,
  ?GPRMA_LATITUDE_TYPE/binary,
  ?GPRMA_LONGITUDE/binary,
  ?GPRMA_LONGITUDE_TYPE/binary,
  ?GPRMA_SPEED/binary,
  ?GPRMA_COURSE/binary,
  ?GPRMA_ANY/binary
  >>
).

-define(TRCCR_HEADER, <<"\\$TRCCR,">>). %%
-define(TRCCR_DATE, <<"(\\d{4})(\\d{2})(\\d{2})">>). %% Date (YYYYMMDD)
-define(TRCCR_TIME, <<"(\\d{2})(\\d{2})(\\d{2})\\.?\\d*,">>). %% Time (HHMMSS.SSS)
-define(TRCCR_VALIDITY, <<"([AV]),">>). %% Validity
-define(TRCCR_LATITUDE, <<"(-?\\d+\\.\\d+),">>). %% Latitude
-define(TRCCR_LONGITUDE, <<"(-?\\d+\\.\\d+),">>). %% Longitude
-define(TRCCR_SPEED, <<"(\\d+\\.\\d+),">>). %% Speed
-define(TRCCR_COURSE, <<"(\\d+\\.\\d+),">>). %% Course
-define(TRCCR_ALTITUDE, <<"(-?\\d+\\.\\d+),">>). %% Altitude
-define(TRCCR_BATTERY, <<"(\\d+\\.?\\d*),">>). %% Battery
-define(TRCCR_ANY, <<".+">>). %%

-define(TRCCR_PATTERN,
  <<
  ?TRCCR_HEADER/binary,
  ?TRCCR_DATE/binary,
  ?TRCCR_TIME/binary,
  ?TRCCR_VALIDITY/binary,
  ?TRCCR_LATITUDE/binary,
  ?TRCCR_LONGITUDE/binary,
  ?TRCCR_SPEED/binary,
  ?TRCCR_COURSE/binary,
  ?TRCCR_ALTITUDE/binary,
  ?TRCCR_BATTERY/binary,
  ?TRCCR_ANY/binary
  >>
).

-record(state, {protocol, transport, socket, timeout, device}).

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
  ok = ranch:accept_ack(Ref),
  Protocol = proplists:get_value(protocol, Opts),
  loop(#state{protocol = Protocol, socket = Socket, transport = Transport}).

loop(State = #state{protocol = Protocol, socket = Socket, transport = Transport, device = Device}) ->
  case Transport:recv(Socket, 0, 5000) of
    {ok, Data} ->
      em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_hardware:resolve(Socket), Data]),
      case parse(Data) of
        {<<"PGID">>, Imei} ->
          em_logger:info("[packet] unit: ip = '~s' imei = '~s'", [em_hardware:resolve(Socket), Imei]),
          case em_data_manager:get_device_by_uid(Imei) of
              null ->
                  em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_hardware:resolve(Socket), Imei]),
                  Transport:close(Socket);
              Object ->
                  loop(State#state{device = Object})
          end;
        {<<"PCPTI">>, Imei} ->
          em_logger:info("[packet] unit: ip = '~s' imei = '~s'", [em_hardware:resolve(Socket), Imei]),
          case em_data_manager:get_device_by_uid(Imei) of
              null ->
                  em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_hardware:resolve(Socket), Imei]),
                  Transport:close(Socket);
              Object ->
                  loop(State#state{device = Object})
          end;
        {<<"GPFID">>, Imei} ->
          em_logger:info("[packet] unit: ip = '~s' imei = '~s'", [em_hardware:resolve(Socket), Imei]),
          case em_data_manager:get_device_by_uid(Imei) of
              null ->
                  em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_hardware:resolve(Socket), Imei]),
                  Transport:close(Socket);
              Object ->
                  loop(State#state{device = Object})
          end;
        {<<"IMEI">>, Imei} ->
          em_logger:info("[packet] unit: ip = '~s' imei = '~s'", [em_hardware:resolve(Socket), Imei]),
          case em_data_manager:get_device_by_uid(Imei) of
              null ->
                  em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_hardware:resolve(Socket), Imei]),
                  Transport:close(Socket);
              Object ->
                  loop(State#state{device = Object})
          end;
        {<<"GPRMC">>, Message} ->
          em_logger:info("[packet] unit: ip = '~s' imei = '~s' message: ~w", [em_hardware:resolve(Socket), maps:get(uniqueId, Device), Message]),
          em_data_manager:create_message(maps:get(id, Device), Protocol, maps:merge(#{imei => maps:get(uniqueId, Device)}, Message)),
          Transport:send(Socket, <<"OK1\r\n">>),
          loop(State);
        {<<"GPGGA">>, Message} ->
          em_logger:info("[packet] unit: ip = '~s' imei = '~s' message: ~w", [em_hardware:resolve(Socket), maps:get(uniqueId, Device), Message]),
          em_data_manager:create_message(maps:get(id, Device), Protocol, maps:merge(#{imei => maps:get(uniqueId, Device)}, Message)),
          Transport:send(Socket, <<"OK1\r\n">>),
          loop(State);
        {<<"GPRMA">>, Message} ->
          em_logger:info("[packet] unit: ip = '~s' imei = '~s' message: ~w", [em_hardware:resolve(Socket), maps:get(uniqueId, Device), Message]),
          em_data_manager:create_message(maps:get(id, Device), Protocol, maps:merge(#{imei => maps:get(uniqueId, Device)}, Message)),
          Transport:send(Socket, <<"OK1\r\n">>),
          loop(State);
        {<<"TRCCR">>, Message} ->
          em_logger:info("[packet] unit: ip = '~s' imei = '~s' message: ~w", [em_hardware:resolve(Socket), maps:get(uniqueId, Device), Message]),
          em_data_manager:create_message(maps:get(id, Device), Protocol, maps:merge(#{imei => maps:get(uniqueId, Device)}, Message)),
          Transport:send(Socket, <<"OK1\r\n">>),
          loop(State)
      end;
    _ ->
      Transport:close(Socket)
  end.

% echo "5. t55"
% (echo -n -e "\$PGID,123456789012345*0F\r\n\$GPRMC,120500.000,A,6000.0000,N,13000.0000,E,0.00,0.00,010112,,*33\r\n";) | nc -v localhost 5005
parse(Data) ->
  case em_regexp:match(Data, ?PATTERN) of
    {match, [_, Header | _]} ->
      io:format("match packet: ~s~n", [Header]),
      case Header of
        <<"PGID">> ->
          {Header, parse_pgid(Data)};
        <<"PCPTI">> ->
          {Header, parse_pcpti(Data)};
        <<"GPFID">> ->
          {Header, parse_gpfid(Data)};
        <<"IMEI">> ->
          {Header, parse_imei(Data)};
        <<"GPRMC">> ->
          {Header, parse_gprmc(Data)};
        <<"GPGGA">> ->
          {Header, parse_gpgga(Data)};
        <<"GPRMA">> ->
          {Header, parse_gprma(Data)};
        <<"TRCCR">> ->
          {Header, parse_trccr(Data)};
        _ ->
         null
      end;
    nomatch ->
      null
  end.

parse_pgid(Data) ->
  case em_regexp:match(Data, ?PGID_PATTERN) of
    {match, [_, Imei|_]} ->
      Imei;
    _ ->
      null
  end.

parse_pcpti(Data) ->
  case em_regexp:match(Data, ?PCPTI_PATTERN) of
    {match, [_, Imei|_]} ->
      Imei;
    _ ->
      null
  end.

parse_gpfid(Data) ->
  case em_regexp:match(Data, ?GPFID_PATTERN) of
    {match, [_, Imei|_]} ->
      Imei;
    _ ->
      null
  end.

parse_imei(Data) ->
  case em_regexp:match(Data, ?IMEI_PATTERN) of
    {match, [_, Imei|_]} ->
      Imei;
    _ ->
      null
  end.

parse_gprmc(Data) ->
  case em_regexp:match(Data, ?GPRMC_PATTERN) of
    {match, [_, Hour, Minute, Second, Validity, LatDD, LatMM_MMMM, LatType, LonDD, LonMM_MMMM, LonType, Speed, Course, Day, Month, Year|_]} ->
      #{
        deviceTime => parse_date(Year, Month, Day, Hour, Minute, Second),
        latitude => parse_coord(LatDD, LatMM_MMMM, LatType),
        longitude => parse_coord(LonDD, LonMM_MMMM, LonType),
        speed => parse_speed(Speed),
        course => parse_course(Course),
        valid => parse_valid(Validity)
      };
    _ ->
      null
  end.


parse_gpgga(Data) ->
  case em_regexp:match(Data, ?GPGGA_PATTERN) of
    {match, [_, _Hour, _Minute, _Second, LatDD, LatMM_MMMM, LatType, LonDD, LonMM_MMMM, LonType|_]} ->
      #{
        %%time => parse_date(Year, Month, Day, Hour, Minute, Second),
        latitude => parse_coord(LatDD, LatMM_MMMM, LatType),
        longitude => parse_coord(LonDD, LonMM_MMMM, LonType)
      };
    _ ->
      null
  end.

parse_gprma(Data) ->
  case em_regexp:match(Data, ?GPRMA_PATTERN) of
    {match, [_, Validity, LatDD, LatMM_MMMM, LatType, LonDD, LonMM_MMMM, LonType, Speed, Course|_]} ->
      #{
        %%time => parse_date(Year, Month, Day, Hour, Minute, Second),
        latitude => parse_coord(LatDD, LatMM_MMMM, LatType),
        longitude => parse_coord(LonDD, LonMM_MMMM, LonType),
        speed => parse_speed(Speed),
        course => parse_course(Course),
        valid => parse_valid(Validity)
      };
    _ ->
      null
  end.

parse_trccr(Data) ->
    %% $TRCCR,20150727040136.279,A,53.897743,27.442885,0.00,0.00,0.00,34,*34
  case em_regexp:match(Data, ?TRCCR_PATTERN) of
    {match, [_, Year, Month, Day, Hour, Minute, Second, Validity, Lat, Lon, Speed, Course, Altitude, Battery|_]} ->
      #{
        deviceTime => trccr_parse_date(Year, Month, Day, Hour, Minute, Second),
        latitude => list_to_float(binary_to_list(Lat)),
        longitude => list_to_float(binary_to_list(Lon)),
        speed => parse_speed(Speed),
        course => parse_course(Course),
        valid => parse_valid(Validity),
        altitude => parse_altitude(Altitude),
        battery => parse_battery(Battery)
      };
    _ ->
      null
  end.

parse_battery(Battery) ->
  bin_to_num(Battery).

parse_altitude(Altitude) ->
  bin_to_num(Altitude).

parse_speed(Speed) ->
  bin_to_num(Speed).

parse_course(Course) ->
  bin_to_num(Course).

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

parse_valid(Validity) when Validity == <<"A">> ->
  true;
parse_valid(Validity) when Validity == <<"V">> ->
  false.

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
  em_hardware:datetime_to_utc(Date).


trccr_parse_date(Year, Month, Day, Hour, Minute, Second) ->
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
  em_hardware:datetime_to_utc(Date).

bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error, no_float} ->
            list_to_integer(N);
        {F, _Rest} ->
            F
    end.

test() ->
  Packets = <<"\$PGID,123456789012345*0F\r\n\$TRCCR,20150727040136.279,A,53.897743,27.442885,0.00,0.00,0.00,34,*34\r\n">>,
  %%Packets = <<"\$PGID,123456789012345*0F\r\n\$GPRMC,120500.000,A,6000.0000,N,13000.0000,E,0.00,0.00,010112,,*33\r\n">>,
  lists:foreach(fun(Packet) ->
    io:format("~s~n", [Packet]),
    parse(Packet)
  end, binary:split(Packets, [<<"\r\n">>], [global])).
