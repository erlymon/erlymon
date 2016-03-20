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

%% API
-export([
  init/0
]).

-export([
  get_server/0,
  update_server/1
]).

-export([
  create_user/1,
  create_user/3,
  update_user/1,
  update_user/3,
  delete_user/1,
  check_user/2,
  get_users/0
]).

-export([
  link_device/2,
  unlink_device/1,
  create_device/1,
  create_device/3,
  update_device/1,
  update_device/4,
  delete_device/1,
  delete_device/2,
  get_devices/1,
  get_devices/2,
  get_device_by_uid/1,
  get_all_devices/0
]).

-export([
  create_message/3,
  get_messages/3,
  get_last_message/2
]).



init() ->
    em_storage:ensure_index(users, #{<<"key">> => #{<<"id">> => 1}, <<"name">> => <<"id_1">>, <<"unique">> => true}),
    em_storage:ensure_index(users, #{<<"key">> => #{<<"email">> => 1}, <<"name">> => <<"email_1">>, <<"unique">> => true}),

    em_storage:ensure_index(devices, #{<<"key">> => #{<<"id">> => 1}, <<"name">> => <<"id_1">>, <<"unique">> => true}),
    em_storage:ensure_index(devices, #{<<"key">> => #{<<"uniqueId">> => 1}, <<"name">> => <<"uniqueId_1">>, <<"unique">> => true}),

    em_storage:ensure_index(servers, #{<<"key">> => #{<<"id">> => 1}, <<"name">> => <<"id_1">>, <<"unique">> => true}),

    em_storage:ensure_index(messages, #{<<"key">> => #{<<"id">> => 1}, <<"name">> => <<"id_1">>, <<"unique">> => true}),
    em_storage:ensure_index(messages, #{<<"key">> => #{<<"deviceId">> => 1, <<"fixTime">> => 1, <<"imei">> => 1}, <<"name">> => <<"deviceId_1_fixTime_1_imei_1">>, <<"unique">> => true}),
    case em_storage_server:get() of
        null ->
            em_storage_server:create(true, 0, 0, 0),
            Admin = em_storage_user:create(<<"admin">>, <<"admin">>, <<"admin">>, true),
            Device = em_storage_device:create(<<"test1">>, <<"123456789012345">>),
            link_device(maps:get(<<"id">>, Admin), maps:get(<<"id">>, Device)),
            em_logger:info("Create account admin: username: ~s, password: ~s", [maps:get(<<"email">>, Admin), maps:get(<<"password">>, Admin)]);
        _ ->
            em_logger:info("Storage already init")
    end.


get_server() ->
    em_storage_server:get().

update_server(Server) ->
    ServerId = maps:get(<<"id">>, Server),
    em_storage_server:update(ServerId, maps:remove(<<"id">>, Server)).

create_message(DeviceId, Protocol, MessageParams) ->
    case em_storage_message:get(DeviceId, maps:get(<<"deviceTime">>, MessageParams)) of
      #{} ->
        case em_storage_message:create(DeviceId, Protocol, MessageParams) of
          null ->
            null;
          Message ->
            em_storage_device:update(DeviceId, #{
              <<"positionId">> => maps:get(<<"id">>, Message),
              <<"lastUpdate">> => bson:unixtime_to_secs(bson:timenow())
            }),
            Message
        end;
      _ ->
        null
    end.

get_messages(DeviceId, TimeFrom, TimeTo) ->
    em_logger:info("get_messages => Device ID: ~w, From: ~w, To: ~w", [DeviceId, TimeFrom, TimeTo]),
    GetMessage = fun(Message, Acc) ->
                        %% deviceTime: "2016-01-09T14:56:18.000+0000"
                        %% fixTime: "2016-01-09T14:56:18.000+0000"
                        %% serverTime: "2016-01-09T14:57:16.000+0000"
                        {ok, DeviceTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, maps:get(<<"deviceTime">>, Message) / 1000),
                        {ok, FixTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, maps:get(<<"fixTime">>, Message) / 1000),
                        {ok, ServerTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, maps:get(<<"serverTime">>, Message) / 1000),
                        NewMessage0 = maps:put(<<"deviceTime">>, DeviceTime, Message),
                        NewMessage1 = maps:put(<<"fixTime">>, FixTime, NewMessage0),
                        NewMessage2 = maps:put(<<"serverTime">>, ServerTime, NewMessage1),
                        [NewMessage2|Acc]
          end,
    Cursor = em_storage_message:get(DeviceId, TimeFrom, TimeTo),
    Messages = em_storage_cursor:foldl(GetMessage, [], Cursor),
    em_storage_cursor:close(Cursor),
    Messages.

get_last_message(MessageId, DeviceId) ->
    Message = em_storage_message:get(#{<<"id">> => MessageId, <<"deviceId">> => DeviceId}),
    case (maps:size(Message) =:= 0) of
        true ->
            null;
        false ->
          %% deviceTime: "2016-01-09T14:56:18.000+0000"
          %% fixTime: "2016-01-09T14:56:18.000+0000"
          %% serverTime: "2016-01-09T14:57:16.000+0000"
          {ok, DeviceTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, maps:get(<<"deviceTime">>, Message) / 1000),
          {ok, FixTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, maps:get(<<"fixTime">>, Message) / 1000),
          {ok, ServerTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, maps:get(<<"serverTime">>, Message) / 1000),
          NewMessage0 = maps:put(<<"deviceTime">>, DeviceTime, Message),
          NewMessage1 = maps:put(<<"fixTime">>, FixTime, NewMessage0),
          NewMessage2 = maps:put(<<"serverTime">>, ServerTime, NewMessage1),
          NewMessage2
    end.


create_user(User) ->
    create_user(maps:get(<<"name">>, User), maps:get(<<"email">>, User), maps:get(<<"password">>, User)).

create_user(Name, Email, Password) ->
    case em_storage_user:get_by_email(Email) of
        null ->
            case em_storage_user:create(Name, Email, Password) of
              null ->
                false;
              _ ->
                true
            end;
        _ ->
            false
    end.

update_user(User) ->
    UserId = maps:get(<<"id">>, User),
    em_storage_user:update(UserId, maps:remove(<<"id">>, User)).



update_user(UserId, Field, Value) ->
    em_storage_user:update(UserId, Field, Value).

delete_user(User) ->
    em_storage_user:delete(maps:get(<<"id">>, User)).

check_user(Email, Password) ->
    em_storage_user:get(Email, em_password:hash(Password)).

get_users() ->
    GetUser = fun(User, Acc) ->
                      [User|Acc]
          end,
    Cursor = em_storage_user:get_all(),
    Users = em_storage_cursor:foldl(GetUser, [], Cursor),
    em_storage_cursor:close(Cursor),
    Users.



create_device(Device) ->
    em_storage_device:create(maps:get(<<"name">>, Device), maps:get(<<"uniqueId">>, Device)).

create_device(UserId, DeviceName, DeviceUniqueId) ->
    case em_storage_device:create(DeviceName, DeviceUniqueId) of
        null ->
            null;
        Device = #{<<"id">> := DeviceId} ->
            em_storage_permission:create(UserId, DeviceId),
            Device
    end.

update_device(Device) ->
    DeviceId = maps:get(<<"id">>, Device),
    em_storage_device:update(DeviceId, maps:remove(<<"id">>, Device)).

update_device(UserId, DeviceId, Field, Value) ->
    case em_storage_permission:get(UserId, DeviceId) of
        null ->
            false;
        _ ->
            em_storage_device:update(DeviceId, Field, Value),
            true
    end.

delete_device(Device) ->
    em_storage_device:delete(maps:get(<<"id">>, Device)).

delete_device(UserId, DeviceId) ->
    case em_storage_permission:get(UserId, DeviceId) of
        null ->
            false;
        _ ->
            em_storage_device:delete(DeviceId),
            em_storage_permission:delete(DeviceId),
            true
    end.


get_devices(UserId) ->
    get_devices(UserId, #{<<"_id">> => false}).

get_devices(UserId, Projector) ->
    GetDeviceById = fun(#{<<"deviceId">> := DeviceId}, Acc) ->
                  case em_storage_device:get_by_id(DeviceId, Projector) of
                      null ->
                          Acc;
                      Device ->
                          %% format lastUpdate: 2016-01-09T15:31:11.000+0000
			                    %%em_logger:info("lastUpdate: ~w", [maps:get(<<"lastUpdate">>, Device)]),
			                    {ok, Date} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, maps:get(<<"lastUpdate">>, Device)),
                          [maps:put(<<"lastUpdate">>, Date, Device)|Acc]
                  end
          end,
    Cursor = em_storage_permission:get(UserId),
    Devices = em_storage_cursor:foldl(GetDeviceById, [], Cursor),
    em_storage_cursor:close(Cursor),
    Devices.

get_device_by_uid(UniqueId) ->
    em_storage_device:get_by_uid(UniqueId).


get_all_devices() ->
    Cursor = em_storage_device:get_all(),
    Devices = em_storage_cursor:map(fun(Device) ->
	      {ok, Date} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, maps:get(<<"lastUpdate">>, Device)),
        maps:put(<<"lastUpdate">>, Date, Device)
    end, Cursor),
    em_storage_cursor:close(Cursor),
    Devices.

link_device(UserId, DeviceId) ->
    em_storage_permission:create(UserId, DeviceId).

unlink_device(DeviceId) ->
    em_storage_permission:delete(DeviceId).
