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

-module(em_storage_permission).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API
-export([
  create/2,
  update/2,
  delete/2,
  get/2,
  get_by_user_id/1,
  get_by_device_id/1
]).

%%// permission
%%{
%%  userId: {type: integer},
%%  deviceId: {type: integer}
%%}

create(UserId, DeviceId) ->
    PermissionModel = #{<<"userId">> => UserId, <<"deviceId">> => DeviceId},
    {_, Item} = em_storage:insert(<<"permissions">>, PermissionModel),
    Item.

update(_UserId, _DeviceId) -> ok.

delete(UserId, DeviceId) ->
    PermissionModel = #{<<"userId">> => UserId, <<"deviceId">> => DeviceId},
    em_storage:delete(<<"permissions">>, PermissionModel).

get(UserId, DeviceId) ->
    Item = em_storage:find_one(<<"permissions">>, #{<<"userId">> => UserId, <<"deviceId">> => DeviceId}, #{projector => #{<<"_id">> => false}}),
    case (maps:size(Item) =/= 0) of
      true ->
	Item;
      false ->
	null
    end.

get_by_user_id(UserId) ->
    em_storage:find(<<"permissions">>, #{<<"userId">> => UserId}, #{projector => #{<<"_id">> => false}}).

get_by_device_id(DeviceId) ->
  em_storage:find(<<"permissions">>, #{<<"deviceId">> => DeviceId}, #{projector => #{<<"_id">> => false}}).
