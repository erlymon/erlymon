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
-module(em_gt06_protocol_SUITE).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-compile(export_all).

-include_lib("em_records.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(MSG_LOGIN, 16#01).
-define(MSG_GPS_LBS_1, 16#12).
-define(MSG_STATUS,16#13).

all() ->
  [
    % список тестов
    parse_test
  ].

init_per_suite(Config) ->
  % действия, выполняемые перед запуском набора тестов
  application:ensure_all_started(erlymon),
  Config.

init_per_testcase(_, Config) ->
  % действия, выполняемые перед запуском теста
  Config.

end_per_testcase(_, Config) ->
  % действия, выполняемые после завершения теста
  Config.

end_per_suite(Config) ->
  % действия, выполняемые после завершения всего набора тестов
  %%em_regexp:stop(),
  Config.

% код тестов

% обратите внимание, что ни одно из объявлений
% init_*/end_* функций не является обязательным

decode_test(_Config) ->
  %% login data
  Sample = {ok, {?MSG_LOGIN, 7, <<"358899054995322">>}},
  Res = em_gt06_protocol:decode(#position{}, <<120,120,13,1,3,88,137,144,84,153,83,34,0,7,244,186,13,10>>),
  ?assert(Sample =:= Res),

  %% gps data
  Sample = {ok,{?MSG_GPS_LBS_1,40,{position,0,<<>>,<<>>,0,1490964614,0,0,false,true,-28.526484444444446,77.19481777777777,0.0,0,216,<<>>,#{<<115,97,116>> => 9}}}},
  Res = em_gt06_protocol:decode(#position{}, <<120,120,31,18,17,3,31,12,50,14,201,3,15,128,216,8,72,56,16,0,84,216,1,148,10,1,3,0,71,81,0,40,94,232,13,10>>),
  em_logger:info("RES: ~w", [Res]),
  Res =:= Sample.
