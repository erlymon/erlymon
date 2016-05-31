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

-module(em_helper_time).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API
-export([
  datetime_to_utc/1,
  utc_to_datetime/1,
  format/2,
  parse/2,
  timestamp/0
]).

format(Format, Utc) ->
  tempo:format_unix(Format, Utc div 1000000).

parse(Format, Bin) ->
  tempo:parse_unix(Format, Bin) * 1000000.

datetime_to_utc({{Year, Month, Day}, Time}) ->
  BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  (calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, Time}) - BaseDate) * 1000000.


utc_to_datetime(Milliseconds) when is_integer(Milliseconds) ->
  BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  Seconds = BaseDate + (Milliseconds div 1000000),
  {{Year, Month, Day}, Time} = calendar:gregorian_seconds_to_datetime(Seconds),
  {{Year, Month, Day}, Time}.


%%--------------------------------------------------------------------
%% @doc Calc current utc time
%% @spec timestamp() -> integer().
%% @end
%%--------------------------------------------------------------------
timestamp() ->
  timer:now_diff(os:timestamp(), {0, 0, 0}).