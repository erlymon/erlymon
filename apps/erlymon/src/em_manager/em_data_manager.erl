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
-module(em_data_manager).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-include("em_records.hrl").

%% API
-export([
  get_server/0,
  update_server/1
]).

-export([
  create_user/1,
  update_user/1,
  delete_user/1,
  check_user/2,
  get_users/0
]).

-export([
  link_device/1,
  unlink_device/1,
  create_device/1,
  update_device/1,
  delete_device/1,
  get_devices/1,
  get_device_by_uid/1,
  get_all_devices/0
]).

-export([
  create_position/2,
  get_positions/3,
  get_last_position/2
]).

-spec(get_server() -> {ok, #server{}} | {error, string()}).
get_server() ->
  em_manager_server:get().

-spec(update_server(Rec :: #server{}) -> {ok, #server{}} | {error, string() | [string()]}).
update_server(Server) ->
  em_manager_server:update(Server).

-spec(create_position(DeviceModel :: #device{}, PositionModel :: #position{}) -> {ok, #position{}} | {error, string()}).
create_position(DeviceModel, PositionModel) ->
      case em_storage:create_position(PositionModel) of
        {ok, Position} ->
          em_manager_devices:update(DeviceModel#device{
            positionId = Position#position.id,
            lastUpdate = em_helper_time:timestamp()
          }),
          case em_manager_devices:get_by_id(DeviceModel#device.id) of
            {ok, Device} ->
              em_manager_event:broadcast(Device, Position),
              {ok, Position};
            _ ->
              {ok, Position}
          end;
        Reason ->
          Reason
      end.

-spec(get_positions(DeviceId :: integer(), TimeFrom :: integer(), TimeTo :: integer()) -> {ok, [#position{}]} | {error, string()}).
get_positions(DeviceId, TimeFrom, TimeTo) ->
    em_logger:info("get_positions => DeviceId: ~w TimeFrom: ~w TimeTo: ~w", [DeviceId, TimeFrom, TimeTo]),
    em_storage:get_positions(DeviceId, TimeFrom, TimeTo).

-spec(get_last_position(PositionId :: integer(), DeviceId :: integer()) -> {ok, #position{}} | {error, string()}).
get_last_position(PositionId, DeviceId) ->
    em_storage:get_last_position(PositionId,  DeviceId).

-spec(create_user(User :: #user{}) -> {ok, #user{}} | {error, string()}).
create_user(User) ->
    em_manager_users:create(User).

-spec(update_user(User :: #user{}) -> {ok, #user{}} | {error, string()}).
update_user(User) ->
    em_manager_users:update(User).

-spec(delete_user(User :: #user{}) -> {ok, #user{}} | {error, string()}).
delete_user(User) ->
    em_manager_users:delete(User).

-spec(check_user(Email :: string(), Password :: string()) -> {ok, #user{}} | {error, string()}).
check_user(Email, Password) ->
  em_manager_users:check(Email, Password).

-spec(get_users() -> {ok, [#user{}]} | {error, string()}).
get_users() ->
    em_manager_users:get().

-spec(create_device(Device :: #device{}) -> {ok, #device{}} | {error, string()}).
create_device(Device) ->
  em_manager_devices:create(Device).

-spec(update_device(Device :: #device{}) -> {ok, #device{}} | {error, string()}).
update_device(Device) ->
  em_manager_devices:update(Device).

-spec(delete_device(Device :: #device{}) -> {ok, #device{}} | {error, string()}).
delete_device(Device) ->
  em_manager_devices:delete(Device).

-spec(get_devices(UserId :: integer()) -> {ok, [#device{}]} | {error, string()}).
get_devices(UserId) ->
  Callback = fun(Permission = #permission{deviceId = DeviceId}, Acc) ->
                em_logger:info("PERMISSION: ~w", [Permission]),
                case em_manager_devices:get_by_id(DeviceId) of
                  {ok, Device} ->
                    [Device | Acc];
                  {error, _Reason} ->
                    Acc
                end
             end,
  {ok, Permissions} = em_storage:get_permissions_by_user_id(UserId),
  Devices = lists:foldl(Callback, [], Permissions),
  {ok, Devices}.

-spec(get_device_by_uid(UniqueId :: string()) -> {ok, #device{}} | {error, string()}).
get_device_by_uid(UniqueId) ->
  em_manager_devices:get_by_uid(UniqueId).

-spec(get_all_devices() -> {ok, [#device{}]} | {error, string()}).
get_all_devices() ->
    em_manager_devices:get().

-spec(link_device(Permission :: #permission{}) -> {ok, #permission{}} | {error, string()}).
link_device(Permission) ->
    em_storage:create_permission(Permission).

-spec(unlink_device(Permission :: #permission{}) -> {ok, #permission{}} | {error, string()}).
unlink_device(Permission) ->
  em_storage:delete_permission(Permission).
