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

-module(em_storage_session).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API
-export([
  create/1,
  update/1,
  delete/1
]).


%%  // session
%%{
%%  id: {type: integer},
%%  userId: {type: integer},
%%  time: {type: integer},
%%}

create(UserId) ->
  SessionModel = #{
    id => bson:unixtime_to_secs(bson:timenow()),
    userId => UserId,
    time => bson:unixtime_to_secs(bson:timenow())
  },
  em_storage:insert(sessions, SessionModel).


update(Id) ->
  em_storage:update(sessions, #{id => Id}, #{ set => #{ time => bson:unixtime_to_secs(bson:timenow()) } }).

delete(Id) ->
  em_storage:delete_one(sessions, #{id => Id}).

get(Id) ->
    em_storage:find_one(sessions, #{id => Id}).
