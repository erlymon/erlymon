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
-module(em_storage_device).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API
-export([
  create/2,
  create/1,
  update/2,
  update/3,
  delete/1,
  get_by_id/1,
  get_by_id/2,
  get_by_uid/1,
  get_all/0
]).

%%//device
%%{
%% id: {type: integer},
%% name: {type: string},
%% uniqueId: {type: string},
%% password: {type: string},
%% status: {type: string},
%% lastUpdate: {type: integer},
%% positionId: {type: integer},
%% dataId: {type: integer}
%%}
%% {"uniqueId":"0101","name":"assa","id":628}
%% {"id":10995,"name":"sgdvdc","uniqueId":"cfgbh","status":null,"lastUpdate":"2016-01-24T11:52:15.000+0000","positionId":0}
create(Name, UniqueId) ->
    DeviceModel = #{
      <<"id">> => bson:unixtime_to_secs(bson:timenow()),
      <<"name">> => Name,
      <<"uniqueId">> => UniqueId,
      <<"status">> => <<>>,
      <<"lastUpdate">> => bson:unixtime_to_secs(bson:timenow()),
      <<"positionId">> => 0
     },
    create(DeviceModel).

create(DeviceModel) ->
    {_, Item} = em_storage:insert(<<"devices">>, DeviceModel),
    Item.

update(Id, <<"name">>, Value) ->
    update(Id,  #{<<"name">> => Value});
update(Id, <<"uniqueId">>, Value) ->
    update(Id,  #{<<"uniqueId">> => Value});
update(Id, <<"password">>, Value) ->
    update(Id,  #{<<"password">> => Value}).

update(Id, DeviceModel) ->
    em_storage:update(<<"devices">>, #{<<"id">> => Id}, DeviceModel).

delete(Id) ->
    em_storage:delete_one(<<"devices">>, #{<<"id">> => Id}).

get_by_id(Id) ->
    get_by_id(Id, #{<<"_id">> => false}).

get_by_id(Id, Projector) ->
    get(#{id => Id}, Projector).


get_by_uid(UniqueId) ->
    get(#{<<"uniqueId">> => UniqueId}, #{<<"_id">> => false}).
    
get(Query, Projector) ->
    Item = em_storage:find_one(<<"devices">>, Query, #{projector => Projector}),
    case (maps:size(Item) =/= 0) of
      true ->
        Item;
      false ->
        null
    end.

get_all() ->
    em_storage:find(<<"devices">>, #{}, #{projector => #{<<"_id">> => false}}).
