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

-module(em_model_permission).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-include("em_records.hrl").

%% API
-export([
  to_map/1,
  from_map/1,
  to_str/1,
  create/1,
  delete/1,
  get_by_user_id/1
]).


to_map(Rec) ->
  #{
    <<"userId">> => Rec#permission.userId,
    <<"deviceId">> => Rec#permission.deviceId
  }.

from_map(Map) ->
  #permission{
    userId = maps:get(<<"userId">>, Map, 0),
    deviceId = maps:get(<<"deviceId">>, Map, 0)
  }.

to_str(Rec) ->
  em_json:encode(to_map(Rec)).

create(Rec) ->
  {_, Item} = em_storage:insert(<<"permissions">>, to_map(Rec)),
  {ok, Item}.

delete(Rec = #permission{userId = UserId}) when UserId == 0 ->
  em_storage:delete(<<"permissions">>, #{<<"deviceId">> => Rec#permission.deviceId});
delete(Rec) ->
  em_storage:delete(<<"permissions">>, to_map(Rec)).

get_by_user_id(UserId) ->
  Callback = fun(Permission) -> from_map(Permission)  end,
  Cursor = em_storage:find(<<"permissions">>, #{<<"userId">> => UserId}, #{projector => #{<<"_id">> => false}}),
  Permissions = em_storage_cursor:map(Callback, Cursor),
  em_storage_cursor:close(Cursor),
  {ok, Permissions}.
