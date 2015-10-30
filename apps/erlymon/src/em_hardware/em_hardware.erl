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
-module(em_hardware).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").


%% hardware: hardware library's entry point.

-export([
  gprmc/5,
  to_wgs84/1,
  to_nmea/1,
  parsing_gprmc/1,
  utc_to_datetime/1,
  datetime_to_utc/1
]).

-export([
  crc16_ccitt/1,
  crc8/1,
  crc/1
]).

-export([
  mpers_to_kn/1,
  kn_to_mpers/1,
  kmperh_to_mpers/1,
  knots_to_mps/1,
  timestamp/0
]).

-export([
  resolve/1
]).

%% API

%% @spec gprmc(DateTime::tuple(), Coordinates::tuple(), GpsStatus::list(), Knots::float(), Course::float()) -> Result::binary()
%% DateTime = {{Year::integer(), Month::integer(), Day::integer()}, {Hours::integer(), Minutes::integer(), Secunds::integer()}}
%% Coordinates = {{Longitude::float()}, LongitudeType::list(), {Latitude::float(), LatitudeType::list()}}
%% Result = binary()
%% @doc Created GPRMC string.
gprmc({{Year, Month, Day}, {Hours, Minutes, Secunds}}, {{Longitude, LongitudeType}, {Latitude, LatitudeType}}, GpsStatus, Knots, Course)
  when is_integer(Year) and is_integer(Month) and is_integer(Day) and is_integer(Hours) and is_integer(Secunds) and is_float(Longitude) and is_list(LongitudeType) and is_float(Latitude) and is_list(LatitudeType) and is_list(GpsStatus) and is_float(Knots) and is_float(Course) ->
  [LongitudeString] = io_lib:fwrite("~.4f", [Longitude]),
  [Lon0, Lon1] = string:tokens(LongitudeString, "."),
  [LatitudeString] = io_lib:fwrite("~.4f", [Latitude]),
  [Lat0, Lat1] = string:tokens(LatitudeString, "."),
  Gprmc = io_lib:fwrite("GPRMC,~2..0B~2..0B~2..0B.~3..0B,~s,~4..0B.~4..0B,~s,~5..0B.~4..0B,~s,~.2f,~.2f,~2..0B~2..0B~2..0B,,,",
    [
      Hours,
      Minutes,
      Secunds,
      0,
      GpsStatus,
      list_to_integer(Lat0), list_to_integer(Lat1),
      LatitudeType,
      list_to_integer(Lon0), list_to_integer(Lon1),
      LongitudeType,
      Knots,
      Course,
      Day,
      Month,
      Year
    ]),
  list_to_binary(io_lib:fwrite("$~s*~.16B", [Gprmc, adf_core_checksum:crc(binary_to_list(list_to_binary(Gprmc)))])).

%% @spec to_wgs84(Value::tuple()) -> Result::tuple()
%% Value = {{Longitude::float()}, LongitudeType::list(), {Latitude::float(), LatitudeType::list()}}
%% Result = {Longitude::float(), Latitude::float()}
%% @doc Convert NMEA183 coordinates to WGS84.
to_wgs84({{Longitude, LongitudeType}, {Latitude, LatitudeType}}) ->
  Convert = fun(Nmea, Type) ->
    Degrees = trunc(Nmea / 100) + (Nmea - trunc(Nmea / 100) * 100) / 60,
    case Type of
      'N' ->
        Degrees;
      'S' ->
        -1 * Degrees;
      'E' ->
        Degrees;
      'W' ->
        -1 * Degrees
    end

  end,
  {Convert(Longitude, LongitudeType), Convert(Latitude, LatitudeType)}.

%% @spec to_nmea(Value::tuple()) -> Result::tuple()
%% Value = {Longitude::float(), Latitude::float()}
%% Result = {{Longitude::float(), LongitudeType::list()}, {Latitude::float(), LatitudeType::list()}}
%% @doc Convert WGS84 coordinates to NMEA.
to_nmea({Longitude, Latitude}) ->
  Convert = fun(Value, {Type0, Type1}) ->
    Degrees = trunc(Value),
    Minutes = (Value - Degrees) * 60,
    Result = (Degrees * 100) + Minutes,
    if
      Value > 0 ->
        {abs(Result), Type0};
      true ->
        {abs(Result), Type1}
    end
  end,
  {Convert(Longitude, {"E", "W"}), Convert(Latitude, {"N", "S"})}.

%% @spec parsing_gprmc(Data::list()) -> Result::tuple()
%% Result = {DateTime::tuple(), Coordinates::tuple(), GpsStatus::list(), Knots::float(), Course::float()}
%% DateTime = {{Year::integer(), Month::integer(), Day::integer()},{Hours::integer(), Minutes::integer(), Secunds::integer()}}
%% Coordinates = {{Longitude::float()}, LongitudeType::list(), {Latitude::float(), LatitudeType::list()}}
%% @doc Parsed GPRMC data.
parsing_gprmc(Data) when is_list(Data) ->
  %%[Gprmc, Crc] = string:tokens(Data, "*"),
  {ok, [Hours, Minutes, Secunds, _Milisecunds, GpsStatus, Latitude, LatitudeType, Longitude, LongitudeType, Knots, Course, Day, Month, Year, _Tail], _} = io_lib:fread("$GPRMC,~2s~2s~2s.~3s,~c,~f,~c,~f,~c,~f,~f,~2s~2s~2s,~2s", Data),
  {
    {{list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)}, {list_to_integer(Hours), list_to_integer(Minutes), list_to_integer(Secunds)}},
    {{Longitude, LongitudeType}, {Latitude, LatitudeType}},
    GpsStatus,
    Knots,
    Course
  }.

%% @spec utc_to_datetime(Millisecunds::integer()) -> Result::tuple()
%% Result = {{Year::integer(), Month::integer(), Day::integer()},{Hours::integer(), Minutes::integer(), Secunds::integer()}}
%% @doc Convert utc to datetime.
utc_to_datetime(Milliseconds) when is_integer(Milliseconds) ->
  BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  Seconds = BaseDate + (Milliseconds div 1000),
  {{Year, Month, Day}, Time} = calendar:gregorian_seconds_to_datetime(Seconds),
  {{Year - 2000, Month, Day}, Time}.

%% @spec datetime_to_utc(DateTime::tuple()) -> Result::integer()
%% DateTime = {{Year::integer(), Month::integer(), Day::integer()},{Hours::integer(), Minutes::integer(), Secunds::integer()}}
%% @doc Convert datetime to utc.
datetime_to_utc({{Year, Month, Day}, Time}) ->
  BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  (calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, Time}) - BaseDate) * 1000.


%%--------------------------------------------------------------------
%% @doc Converted meters per secunds to knots.
%% @spec mpers_to_kn(Value::float()) -> Result::float()
%% @end
%%--------------------------------------------------------------------

%% @doc Converted meters per secunds to knots.
mpers_to_kn(Value) when is_float(Value) ->
  Value * 3600 / 1852.

%%--------------------------------------------------------------------
%% @doc Converted knots to meters per secunds.
%% @spec kn_to_mpers(Value::float()) -> Result::float()
%% @end
%%--------------------------------------------------------------------
kn_to_mpers(Value) when is_float(Value) ->
  Value * 1852 / 3600.

%%--------------------------------------------------------------------
%% @doc Converted km per hour to meters per secunds.
%% @spec kmperh_to_mpers(Value::float()) -> Result::float()
%% @end
%%--------------------------------------------------------------------
kmperh_to_mpers(Value) when is_float(Value) ->
  Value * 1000 / 3600.


%% parse knots and convert to mps
%% 1 knots = 0.514 m/s
knots_to_mps(Value) when is_float(Value) ->
  Value * 0.514.

%%--------------------------------------------------------------------
%% @doc Calc current utc time
%% @spec timestamp() -> integer().
%% @end
%%--------------------------------------------------------------------
timestamp() ->
  timer:now_diff(now(), {0, 0, 0}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec crc16_ccitt(Data::binary()) -> Result::integer()
%% @doc Caclulate crc.
%%  Name  : CRC-16 CCITT
%%  Poly  : 0x8408
%%  Init  : 0xFFFF
%%  Revert: false
%%  XorOut: 0x0000
%%  Check : 0x6F91 ("123456789")
%%  MaxLen: 4095 bytes (32767 bits) - detection single, double, triple, and all odd errors
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crc16_ccitt(Data) when is_binary(Data) ->
  crc16_ccitt(binary_to_list(Data), 16#ffff).

crc16_ccitt([], Crc) ->
  Crc;
crc16_ccitt([Head | Tail], Crc) ->
  NewCrc = Crc bxor ((Head bsl 8) band 16#ffff),
  crc16_ccitt(Tail, crc16_ccitt_loop(8, NewCrc)).

crc16_ccitt_loop(Count, Crc) when Count == 0 ->
  Crc;
crc16_ccitt_loop(Count, Crc) ->
  NewCrc = ((Crc bsl 1) band 16#ffff),
  case (Crc band 16#8000) of
    0 ->
      crc16_ccitt_loop(Count - 1, NewCrc);
    _ ->
      crc16_ccitt_loop(Count - 1, NewCrc bxor 16#1021)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec crc8(Data::binary()) -> Result::integer()
%% @doc Calculate crc.
%%%  Name  : CRC-8
%%%  Poly  : 0x31    x^8 + x^5 + x^4 + 1
%%%  Init  : 0xFF
%%%  Revert: false
%%%  XorOut: 0x00
%%%  Check : 0xF7 ("123456789")
%%%  MaxLen: 15 bytes(127 bits) - detection single, double, triple,  and all odd errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crc8(Data) when is_binary(Data) ->
  crc8(binary_to_list(Data), 16#ff).

crc8([], Crc) ->
  Crc;
crc8([Head | Tail], Crc) ->
  NewCrc = Crc bxor Head,
  crc8(Tail, crc8_loop(8, NewCrc)).

crc8_loop(Count, Crc) when Count == 0 ->
  Crc;
crc8_loop(Count, Crc) ->
  NewCrc = (Crc bsl 1) band 16#ff,
  case (Crc band 16#80) of
    0 ->
      crc8_loop(Count - 1, NewCrc);
    _ ->
      crc8_loop(Count - 1, (NewCrc bxor 16#31))
  end.


%% @spec crc(Data::list()) -> Result::integer()
%% @doc Calculate crc.
crc(Data) when is_list(Data) ->
  crc(Data, 0).

crc([], Crc) ->
  Crc;
crc([Head | Tail], Crc) ->
  crc(Tail, (Crc bxor Head) band 16#ff).




resolve(Socket) ->
  case inet:peername(Socket) of
    {ok, {Address, _Port}} ->
      list_to_binary(io_lib:format("~w.~w.~w.~w", tuple_to_list(Address)));
    {error, _} ->
      <<"unknown">>
  end.
%% End of Module.
