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
-module(em_converter).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-ifdef(TEST).
-compile(export_all).
-endif. % EXPORT_ALL

%% API
-export([
  mpers_to_kn/1,
  kn_to_mpers/1,
  kmperh_to_mpers/1,
  knots_to_mps/1
]).

-export([bin_to_hexstr/1,hexstr_to_bin/1]).

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


%%--------------------------------------------------------------------
%% @doc parse knots and convert to mps
%% 1 knots = 0.514 m/s
%% @spec knots_to_mps(Value :: float()) -> float().
%% @end
%%--------------------------------------------------------------------
-spec knots_to_mps(Value :: float()) -> float().
knots_to_mps(Value) when is_float(Value) ->
  Value * 0.514.


%%--------------------------------------------------------------------
%% @doc Converted binary to hex string
%% @spec bin_to_hexstr(Bin :: binary()) -> list().
%% @end
%%--------------------------------------------------------------------
-spec bin_to_hexstr(Bin :: binary()) -> list().
bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

%%--------------------------------------------------------------------
%% @doc Converted hex string to binary
%% @spec hexstr_to_bin(String :: list()) -> binary().
%% @end
%%--------------------------------------------------------------------
-spec hexstr_to_bin(String :: list()) -> binary().
hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]);
hexstr_to_bin([X|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", lists:flatten([X,"0"])),
  hexstr_to_bin(T, [V | Acc]).