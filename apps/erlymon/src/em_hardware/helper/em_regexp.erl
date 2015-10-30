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
-module(em_regexp).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API
-export([match/2]).

-spec match(Data :: binary(), Pattern :: binary()) -> ok.
match(Data, Pattern) ->
  {ok, PatternCompile} = re:compile(Pattern),
  case re:run(Data, PatternCompile) of
    {match, List} ->
      Res = lists:reverse(lists:foldl(fun(Param, Res) ->
        [read_param(Param, Data) | Res]
      end, [], List)),
      {match, Res};
    _ ->
      nomatch
  end.

read_param({-1, 0}, _) ->
  void;
read_param({Pos, Len}, Data) ->
  binary:part(Data, Pos, Len).
