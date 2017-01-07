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
-module(em_checksum).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").


%% API
-export([
  sum/1
]).

-export([
  crc16_ccitt/1,
  crc8/1,
  crc/1
]).

-spec(sum(Data :: string()) -> integer()).
sum(Data) ->
  Bytes = binary:bin_to_list(Data),
  Fun = fun(Byte, Acc) ->
    (Acc + Byte) band 16#ff
    end,
  lists:foldl(Fun, 0, Bytes).



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