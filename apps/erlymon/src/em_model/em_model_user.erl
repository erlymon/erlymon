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

-module(em_model_user).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-include("em_records.hrl").

%% API
-export([
  to_map/1,
  from_map/1,
  to_str/1]).


to_map(Rec) ->
  #{
    id => Rec#device.id,
    name => Rec#device.name,
    uniqueId => Rec#device.uniqueId,
    status => Rec#device.status,
    lastUpdate => Rec#device.lastUpdate,
    positionId => Rec#device.positionId
  }.

from_map(Map) ->
  #device{
    id = maps:get(id, Map, 0),
    name = maps:get(name, Map, <<"">>),
    uniqueId = maps:get(uniqueId, Map, <<"">>),
    status = maps:get(status, Map, ?STATUS_UNKNOWN),
    lastUpdate = maps:get(lastUpdate, Map, 0),
    positionId = maps:get(positionId, Map, 0)
  }.

to_str(Rec) ->
  em_json:encode(to_map(Rec)).
