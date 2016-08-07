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

-module(em_model_position).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-include("em_records.hrl").

%% API
-export([
  create/1,
  to_map/1,
  from_map/1,
  to_str/1]).


to_map(Rec) ->
  #{
    id => Rec#position.id,
    type => Rec#position.type,
    protocol => Rec#position.protocol,
    serverTime => Rec#position.serverTime,
    deviceTime => Rec#position.deviceTime,
    fixTime => Rec#position.fixTime,
    deviceId => Rec#position.deviceId,
    outdated => Rec#position.outdated,
    valid => Rec#position.valid,
    latitude => Rec#position.latitude,
    longitude => Rec#position.longitude,
    altitude => Rec#position.altitude,
    speed => Rec#position.speed,
    course => Rec#position.course,
    address => Rec#position.address,
    attributes => Rec#position.attributes
  }.

from_map(Map) ->
  #position{
    id = maps:get(id, Map, 0),
    type = maps:get(type, Map, <<"">>),
    protocol = maps:get(protocol, Map, <<"">>),
    serverTime = maps:get(serverTime, Map, 0),
    deviceTime = maps:get(deviceTime, Map, 0),
    fixTime = maps:get(fixTime, Map, 0),
    deviceId = maps:get(deviceId, Map, 0),
    outdated = maps:get(outdated, Map, false),
    valid = maps:get(valid, Map, false),
    latitude = maps:get(latitude, Map, 0.0),
    longitude = maps:get(longitude, Map, 0.0),
    altitude = maps:get(altitude, Map, 0.0),
    speed = maps:get(speed, Map, 0.0),
    course = maps:get(course, Map, 0.0),
    address = maps:get(address, Map, <<"">>),
    attributes = maps:get(attributes, Map, #{})
  }.

to_str(Rec) ->
  em_json:encode(to_map(Rec)).

create(Rec) ->
  ServerTime = em_helper_time:timestamp(),
  DeviceTime = Rec#position.deviceTime,
  MessageModel = Rec#position{
    id = em_helper_time:timestamp() div 1000000,
    type = <<>>,
    serverTime = ServerTime,
    fixTime = fix_time(ServerTime, DeviceTime),
    deviceTime = DeviceTime
  },
  em_logger:info("Message: ~s", [to_str(MessageModel)]),
  {_, Item} = em_storage:insert(<<"messages">>, to_map(MessageModel)),
  from_map(Item).


fix_time(ServerTime, DeviceTime) when ServerTime < DeviceTime ->
  ServerTime;
fix_time(_, DeviceTime) ->
  DeviceTime.