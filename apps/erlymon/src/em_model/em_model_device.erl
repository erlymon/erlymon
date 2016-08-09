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

-module(em_model_device).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-include("em_records.hrl").

%% API
-export([
  to_map/1,
  from_map/1,
  to_str/1,
  get_all/0,
  get_devices/1,
  get_by_uid/1,
  get_by_id/1
]).


to_map(Rec) ->
  #{
    <<"id">> => Rec#device.id,
    <<"name">> => Rec#device.name,
    <<"uniqueId">> => Rec#device.uniqueId,
    <<"status">> => Rec#device.status,
    <<"lastUpdate">> => Rec#device.lastUpdate,
    <<"positionId">> => Rec#device.positionId
  }.

from_map(Map) ->
  #device{
    id = maps:get(<<"id">>, Map, 0),
    name = maps:get(<<"name">>, Map, <<"">>),
    uniqueId = maps:get(<<"uniqueId">>, Map, <<"">>),
    status = maps:get(<<"status">>, Map, ?STATUS_UNKNOWN),
    lastUpdate = maps:get(<<"lastUpdate">>, Map, 0),
    positionId = maps:get(<<"positionId">>, Map, 0)
  }.

to_str(Recs) when is_list(Recs) ->
  em_logger:info("CONVERT RECORDS: ~w", [Recs]),
  em_json:encode(lists:map(fun(Rec) -> to_map(Rec) end, Recs));
to_str(Rec) ->
  em_logger:info("CONVERT RECORD: ~w", [Rec]),
  em_json:encode(to_map(Rec)).


get_by_uid(UniqueId) ->
  Item = em_storage:find_one(<<"devices">>, #{<<"uniqueId">> => UniqueId}, #{projector => #{<<"_id">> => false, <<"positionId">> => false}}),
  case (maps:size(Item) =/= 0) of
    true ->
      {ok, from_map(Item)};
    false ->
      {error, <<"Not find device by uniqueId">>}
  end.

get_by_id(Id) ->
  Item = em_storage:find_one(<<"devices">>, #{<<"id">> => Id}, #{projector => #{<<"_id">> => false, <<"positionId">> => false}}),
  case (maps:size(Item) =/= 0) of
    true ->
      {ok, from_map(Item)};
    false ->
      {error, <<"Not find device by id">>}
  end.

get_all() ->
  Callback = fun(Device) ->
                Rec = from_map(Device),
                {ok, Date} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, Device#device.lastUpdate),
                Rec#device{lastUpdate = Date}
             end,
  Cursor = em_storage:find(<<"devices">>, #{}, #{projector => #{<<"_id">> => false, <<"positionId">> => false}}),
  Devices = em_storage_cursor:map(Callback, Cursor),
  em_storage_cursor:close(Cursor),
  {ok, Devices}.

get_devices(UserId) ->
  Callback = fun(Permission = #permission{deviceId = DeviceId}, Acc) ->
                em_logger:info("PERMISSION: ~w", [Permission]),
                case get_by_id(DeviceId) of
                  {ok, Device} ->
                    {ok, Date} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, Device#device.lastUpdate),
                    [Device#device{lastUpdate = Date} | Acc];
                  {error, _Reason} ->
                    Acc
                end
             end,
  {ok, Permissions} = em_model_permission:get_by_user_id(UserId),
  Devices = lists:foldl(Callback, [], Permissions),
  {ok, Devices}.