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
  crc16/2,
  crc16_ccitt/1,
  crc8/1,
  crc/1
]).

-define(CRC16_CCITT_TABLE_REVERSE, array:from_list([
  16#0000, 16#1189, 16#2312, 16#329B, 16#4624, 16#57AD, 16#6536, 16#74BF,
  16#8C48, 16#9DC1, 16#AF5A, 16#BED3, 16#CA6C, 16#DBE5, 16#E97E, 16#F8F7,
  16#1081, 16#0108, 16#3393, 16#221A, 16#56A5, 16#472C, 16#75B7, 16#643E,
  16#9CC9, 16#8D40, 16#BFDB, 16#AE52, 16#DAED, 16#CB64, 16#F9FF, 16#E876,
  16#2102, 16#308B, 16#0210, 16#1399, 16#6726, 16#76AF, 16#4434, 16#55BD,
  16#AD4A, 16#BCC3, 16#8E58, 16#9FD1, 16#EB6E, 16#FAE7, 16#C87C, 16#D9F5,
  16#3183, 16#200A, 16#1291, 16#0318, 16#77A7, 16#662E, 16#54B5, 16#453C,
  16#BDCB, 16#AC42, 16#9ED9, 16#8F50, 16#FBEF, 16#EA66, 16#D8FD, 16#C974,
  16#4204, 16#538D, 16#6116, 16#709F, 16#0420, 16#15A9, 16#2732, 16#36BB,
  16#CE4C, 16#DFC5, 16#ED5E, 16#FCD7, 16#8868, 16#99E1, 16#AB7A, 16#BAF3,
  16#5285, 16#430C, 16#7197, 16#601E, 16#14A1, 16#0528, 16#37B3, 16#263A,
  16#DECD, 16#CF44, 16#FDDF, 16#EC56, 16#98E9, 16#8960, 16#BBFB, 16#AA72,
  16#6306, 16#728F, 16#4014, 16#519D, 16#2522, 16#34AB, 16#0630, 16#17B9,
  16#EF4E, 16#FEC7, 16#CC5C, 16#DDD5, 16#A96A, 16#B8E3, 16#8A78, 16#9BF1,
  16#7387, 16#620E, 16#5095, 16#411C, 16#35A3, 16#242A, 16#16B1, 16#0738,
  16#FFCF, 16#EE46, 16#DCDD, 16#CD54, 16#B9EB, 16#A862, 16#9AF9, 16#8B70,
  16#8408, 16#9581, 16#A71A, 16#B693, 16#C22C, 16#D3A5, 16#E13E, 16#F0B7,
  16#0840, 16#19C9, 16#2B52, 16#3ADB, 16#4E64, 16#5FED, 16#6D76, 16#7CFF,
  16#9489, 16#8500, 16#B79B, 16#A612, 16#D2AD, 16#C324, 16#F1BF, 16#E036,
  16#18C1, 16#0948, 16#3BD3, 16#2A5A, 16#5EE5, 16#4F6C, 16#7DF7, 16#6C7E,
  16#A50A, 16#B483, 16#8618, 16#9791, 16#E32E, 16#F2A7, 16#C03C, 16#D1B5,
  16#2942, 16#38CB, 16#0A50, 16#1BD9, 16#6F66, 16#7EEF, 16#4C74, 16#5DFD,
  16#B58B, 16#A402, 16#9699, 16#8710, 16#F3AF, 16#E226, 16#D0BD, 16#C134,
  16#39C3, 16#284A, 16#1AD1, 16#0B58, 16#7FE7, 16#6E6E, 16#5CF5, 16#4D7C,
  16#C60C, 16#D785, 16#E51E, 16#F497, 16#8028, 16#91A1, 16#A33A, 16#B2B3,
  16#4A44, 16#5BCD, 16#6956, 16#78DF, 16#0C60, 16#1DE9, 16#2F72, 16#3EFB,
  16#D68D, 16#C704, 16#F59F, 16#E416, 16#90A9, 16#8120, 16#B3BB, 16#A232,
  16#5AC5, 16#4B4C, 16#79D7, 16#685E, 16#1CE1, 16#0D68, 16#3FF3, 16#2E7A,
  16#E70E, 16#F687, 16#C41C, 16#D595, 16#A12A, 16#B0A3, 16#8238, 16#93B1,
  16#6B46, 16#7ACF, 16#4854, 16#59DD, 16#2D62, 16#3CEB, 16#0E70, 16#1FF9,
  16#F78F, 16#E606, 16#D49D, 16#C514, 16#B1AB, 16#A022, 16#92B9, 16#8330,
  16#7BC7, 16#6A4E, 16#58D5, 16#495C, 16#3DE3, 16#2C6A, 16#1EF1, 16#0F78
])).

-spec(sum(Data :: string()) -> integer()).
sum(Data) ->
  Bytes = binary:bin_to_list(Data),
  Fun = fun(Byte, Acc) ->
    (Acc + Byte) band 16#ff
    end,
  lists:foldl(Fun, 0, Bytes).


-spec(crc16(Type :: atom(), Bin :: binary()) -> integer()).
crc16(crc16_x25, Bin) when is_binary(Bin) ->
  crc16_reflected(binary_to_list(Bin), 16#FFFF, ?CRC16_CCITT_TABLE_REVERSE) bxor 16#FFFF.

crc16_reflected([], InitVal, _)  ->
  InitVal;
crc16_reflected([Head | Tail], InitVal, Table)  ->
  %% crc16 = table[(crc16 ^ buf.get()) & 0xff] ^ (crc16 >> 8);
  Crc16 = array:get((InitVal bxor Head) band 16#ff, Table) bxor (InitVal bsr 8),
  crc16_reflected(Tail, Crc16, Table).


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