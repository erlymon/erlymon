%%%-------------------------------------------------------------------
%%% @author Sergey Penkovsky
%%% @copyright (C) 2015, Sergey Penkovsky <sergey.penkovsky@gmail.com>
%%% @doc
%%%    Erlymon is an open source GPS tracking system for various GPS tracking devices.
%%%
%%%    Copyright (C) 2015, Sergey Penkovsky <sergey.penkovsky@gmail.com>.
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

%% API
-export([
         check_admin/1,
         check_user/2,
         check_device/2
]).

check_admin(UserId) ->
    em_logger:info("check_admin: ~w", [UserId]),
    case em_storage_user:get_by_id(UserId) of
        null ->
            false;
        User ->
            em_logger:info("check_admin user: ~w", [User]),
            maps:get(admin, User)
    end.

check_user(UserId, OtherUserId) when UserId == OtherUserId ->
    false;
check_user(UserId, _) ->
    check_admin(UserId).



check_device(UserId, DeviceId) ->
    case em_storage_permission:get(UserId, DeviceId) of
        null ->
            false;
        _ ->
            true
    end.

