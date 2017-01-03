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
-module(em_permissions_manager).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-include("em_records.hrl").

%% API
-export([
  check_admin/1,
  check_user/2,
  check_device/2,
  check_registration/0
]).

-spec(check_admin(UserId :: integer()) -> boolean()).
check_admin(UserId) ->
    em_logger:info("check_admin: ~w", [UserId]),
    case em_manager_users:get(UserId) of
      {ok, #user{admin = true}} ->
            %%em_logger:info("check_admin user: ~w", [User]),
            true;
      _ ->
        false
    end.

-spec(check_user(UserId :: integer(), OtherUserId :: integer()) -> boolean()).
check_user(UserId, OtherUserId) when UserId == OtherUserId ->
    true;
check_user(UserId, _) ->
    check_admin(UserId).


-spec(check_device(UserId :: integer(), DeviceId :: integer()) -> boolean()).
check_device(UserId, DeviceId) ->
    case em_manager_permissions:get(UserId, DeviceId) of
      {error, _Reason} ->
            false;
      {ok, _Permission} ->
            true
    end.

-spec(check_registration() -> boolean()).
check_registration() ->
  case em_manager_server:get() of
    {ok, #server{registration = true}} ->
      true;
    _ ->
      false
  end.

