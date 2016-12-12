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
-module(em_gl100_protocol_SUITE).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").


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

parse_test(_Config) ->
  Sample = {ok,<<"123456789012345">>,
    {position,0,<<>>,<<>>,0,<<"2016/12/12 21:00:00">>,0,0,false,
      true,60.0,130.0,0.0,0.0,0,<<>>,#{}}},

  Res = em_gl100_protocol:parse(<<"+RESP:GTSOS,123456789012345,0,0,0,1,0.0,0,0.0,1,130.000000,60.000000,20120101120300,0460,0000,18d8,6141,00,11F0,0102120204\0">>),
  io:format("RES: ~w~n", [Res]),
  ?assert(Sample =:= Res).
