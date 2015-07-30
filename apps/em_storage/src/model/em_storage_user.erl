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
-module(em_storage_user).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API
-export([
  create/4,
  create/3,
  create/1,
  update/2,
  delete/1,
  get_by_name/1,
  get_by_email/1,
  get_by_id/1,
  get/2,
  get_all/0
]).


%%  // user
%%{
%%  id: {type: integer},
%%  name: {type: string},
%%  email: {type: string},
%%  password: {type: string},
%%  hashedPassword: {type: string},
%%  salt: {type: string},
%%  admin: {type: boolean},

%%  // user settings
%%  map: {type: string},
%%  language: {type: string},
%%  distanceUnit: {type: string},
%%  speedUnit: {type: string},
%%  latitude: {type: float},
%%  longitude: {type: float},
%%  zoom: {type: float}
%%}

create(Name, Email, Password) ->
  create(Name, Email, Password, false).

create(Name, Email, Password, Admin) ->
  UserModel = #{
    id => bson:unixtime_to_secs(bson:timenow()),
    name => Name,
    password => Password,
    hashPassword => em_password:hash(Password),
    email => Email,
    admin => Admin,
    lastUpdate => bson:unixtime_to_secs(bson:timenow())
  },
  create(UserModel).

create(UserModel) ->
  em_storage:insert(users, UserModel).

update(Id, UserModel) ->
  em_storage:update(users, #{id => Id}, UserModel).

delete(Id) ->
  em_storage:delete_one(users, #{id => Id}).

get_by_name(Name) ->
    em_storage:find_one(users, #{name => Name}).

get_by_email(Email) ->
    em_storage:find_one(users, #{email => Email}).

get_by_id(UserId) ->
    em_storage:find_one(users, #{id => UserId}).

get(Email, HashPassword) ->
    em_storage:find_one(users, #{email => Email, hashPassword => HashPassword}).


get_all() ->
    em_storage:find(users, #{}).
