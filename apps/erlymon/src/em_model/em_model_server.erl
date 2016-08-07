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

-module(em_model_server).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-include("em_records.hrl").

%% API
-export([
  to_map/1,
  from_map/1,
  to_str/1,
  update/1,
  get/0
]).


to_map(Rec) ->
  #{
    <<"id">> => Rec#server.id,
    <<"registration">> => Rec#server.registration,
    <<"readonly">> => Rec#server.readonly,
    <<"map">> => Rec#server.map,
    <<"bingKey">> => Rec#server.bingKey,
    <<"mapUrl">> => Rec#server.mapUrl,
    <<"language">> => Rec#server.language,
    <<"distanceUnit">> => Rec#server.distanceUnit,
    <<"speedUnit">> => Rec#server.speedUnit,
    <<"latitude">> => Rec#server.latitude,
    <<"longitude">> => Rec#server.longitude,
    <<"zoom">> => Rec#server.zoom
  }.

from_map(Map) ->
  #server{
    id = maps:get(<<"id">>, Map, 0),
    registration = maps:get(<<"registration">>, Map, true),
    readonly = maps:get(<<"readonly">>, Map, false),
    map = maps:get(<<"map">>, Map, <<"">>),
    bingKey = maps:get(<<"bingKey">>, Map, <<"">>),
    mapUrl = maps:get(<<"mapUrl">>, Map, <<"">>),
    language = maps:get(<<"language">>, Map, <<"">>),
    distanceUnit = maps:get(<<"distanceUnit">>, Map, <<"">>),
    speedUnit = maps:get(<<"speedUnit">>, Map, <<"">>),
    latitude = maps:get(<<"latitude">>, Map, 0),
    longitude = maps:get(<<"longitude">>, Map, 0),
    zoom = maps:get(<<"zoom">>, Map, 0)
  }.

to_str(Rec) ->
  em_json:encode(to_map(Rec)).

update(Server) ->
  em_storage:update(<<"servers">>, #{<<"id">> => Server#server.id}, maps:remove(<<"id">>, to_map(Server))).

get() ->
  Item = em_storage:find_one(<<"servers">>, #{}, #{projector => #{<<"_id">> => false}}),
  case (maps:size(Item) =/= 0) of
    true ->
      {ok, from_map(Item)};
    false ->
      {error, <<"Not find">>}
  end.
