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

-module(em_storage_server).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API
-export([
  create/4,
  update/2,
  get/0
]).

%%// server
%%{
%%  id: {type: integer},
%%  registration : {type: boolean}
%%  latitude : {type: float}
%%  longitude : {type: float}
%%  zoom : {type: integer}
%%}

create(Registration, Latitude, Longitude, Zoom) ->
    ServerModel = #{
      id => bson:unixtime_to_secs(bson:timenow()),
      registration => Registration,
      latitude => Latitude,
      longitude => Longitude,
      zoom => Zoom
     },
    em_storage:insert(servers, ServerModel).

update(ServerId, ServerModel) ->
    em_storage:update(servers, #{id => ServerId}, ServerModel).

get() ->
    em_storage:find_one(servers, #{}).
