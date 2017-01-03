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
  utc_to_datetime/1,
  datetime_to_utc/1,
  to_timestamp/1
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

%% @spec utc_to_datetime(Millisecunds::integer()) -> Result::tuple()
%% Result = {{Year::integer(), Month::integer(), Day::integer()},{Hours::integer(), Minutes::integer(), Secunds::integer()}}
%% @doc Convert utc to datetime.
-spec utc_to_datetime(integer()) -> tuple().
utc_to_datetime(Milliseconds) when is_integer(Milliseconds) ->
  BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  Seconds = BaseDate + (Milliseconds div 1000),
  {{Year, Month, Day}, Time} = calendar:gregorian_seconds_to_datetime(Seconds),
  {{Year - 2000, Month, Day}, Time}.

%% @spec datetime_to_utc(DateTime::tuple()) -> Result::integer()
%% DateTime = {{Year::integer(), Month::integer(), Day::integer()},{Hours::integer(), Minutes::integer(), Secunds::integer()}}
%% @doc Convert datetime to utc.
-spec datetime_to_utc(tuple()) -> integer().
datetime_to_utc({{Year, Month, Day}, Time}) ->
  BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  (calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, Time}) - BaseDate) * 1000.


%% @spec to_timestamp(DateTime::tuple()) -> Result::integer()
%% DateTime = {{Year::integer(), Month::integer(), Day::integer()},{Hours::integer(), Minutes::integer(), Secunds::integer()}}
%% @doc Convert datetime to utc.
-spec to_timestamp(tuple()) -> integer().
to_timestamp({{Year,Month,Day},{Hours,Minutes,Seconds}}) ->
  (calendar:datetime_to_gregorian_seconds(
    {{Year,Month,Day},{Hours,Minutes,Seconds}}
  ) - 62167219200)*1000000.
%%--------------------------------------------------------------------
%% @doc Converted meters per secunds to knots.
%% @spec mpers_to_kn(Value::float()) -> Result::float()
%% @end
%%--------------------------------------------------------------------

%% @doc Converted meters per secunds to knots.
-spec mpers_to_kn(Value :: float()) -> float().
mpers_to_kn(Value) when is_float(Value) ->
  Value * 3600 / 1852.

%%--------------------------------------------------------------------
%% @doc Converted knots to meters per secunds.
%% @spec kn_to_mpers(Value::float()) -> Result::float()
%% @end
%%--------------------------------------------------------------------
-spec kn_to_mpers(Value :: float()) -> float().
kn_to_mpers(Value) when is_float(Value) ->
  Value * 1852 / 3600.

%%--------------------------------------------------------------------
%% @doc Converted km per hour to meters per secunds.
%% @spec kmperh_to_mpers(Value::float()) -> Result::float()
%% @end
%%--------------------------------------------------------------------
-spec kmperh_to_mpers(Value :: float()) -> float().
kmperh_to_mpers(Value) when is_float(Value) ->
  Value * 1000 / 3600.


%% parse knots and convert to mps
%% 1 knots = 0.514 m/s
-spec knots_to_mps(Value :: float()) -> float().
knots_to_mps(Value) when is_float(Value) ->
  Value * 0.514.

%%--------------------------------------------------------------------
%% @doc Calc current utc time
%% @spec timestamp() -> integer().
%% @end
%%--------------------------------------------------------------------
-spec timestamp() -> integer().
timestamp() ->
  timer:now_diff(os:timestamp(), {0, 0, 0}).


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
-spec crc16_ccitt(Data :: binary()) -> integer().
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
-spec crc8(Data :: binary()) -> integer().
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
-spec crc(Data :: list()) -> integer().
crc(Data) when is_list(Data) ->
  crc(Data, 0).

crc([], Crc) ->
  Crc;
crc([Head | Tail], Crc) ->
  crc(Tail, (Crc bxor Head) band 16#ff).



-spec resolve(Socket :: inet:socket()) -> string().
resolve(Socket) ->
  case inet:peername(Socket) of
    {ok, {Address, _Port}} ->
      list_to_binary(io_lib:format("~w.~w.~w.~w", tuple_to_list(Address)));
    {error, _} ->
      <<"unknown">>
  end.
%% End of Module.
