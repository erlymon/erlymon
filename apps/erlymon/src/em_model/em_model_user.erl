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
  to_str/1,
  get/2,
  get_by_id/1
]).

to_map(Rec) ->
  #{
    <<"id">> => Rec#user.id,
    <<"name">> => Rec#user.name,
    <<"email">> => Rec#user.email,
    <<"readonly">> => Rec#user.readonly,
    <<"admin">> => Rec#user.admin,
    <<"map">> => Rec#user.map,
    <<"language">> => Rec#user.language,
    <<"distanceUnit">> => Rec#user.distanceUnit,
    <<"speedUnit">> => Rec#user.speedUnit,
    <<"latitude">> => Rec#user.latitude,
    <<"longitude">> => Rec#user.longitude,
    <<"zoom">> => Rec#user.zoom,
    <<"password">> => Rec#user.password,
    <<"hashPassword">> => Rec#user.hashPassword,
    <<"salt">> => Rec#user.salt
  }.

from_map(Map) ->
  #user{
    id = maps:get(<<"id">>, Map, 0),
    name = maps:get(<<"name">>, Map, <<"">>),
    email = maps:get(<<"email">>, Map, <<"">>),
    readonly = maps:get(<<"readonly">>, Map, false),
    admin = maps:get(<<"admin">>, Map, false),
    map = maps:get(<<"map">>, Map, <<"">>),
    language = maps:get(<<"language">>, Map, <<"">>),
    distanceUnit = maps:get(<<"distanceUnit">>, Map, <<"">>),
    speedUnit = maps:get(<<"speedUnit">>, Map, <<"">>),
    latitude = maps:get(<<"latitude">>, Map, 0.0),
    longitude = maps:get(<<"longitude">>, Map, 0.0),
    zoom = maps:get(<<"zoom">>, Map, 0.0),
    password = maps:get(<<"password">>, Map, <<"">>),
    hashPassword = maps:get(<<"hashPassword">>, Map, <<"">>),
    salt = maps:get(<<"salt">>, Map, <<"">>)
  }.

to_str(Rec) ->
  em_json:encode(to_map(Rec)).


get_by_id(UserId) ->
  Item = em_storage:find_one(<<"users">>, #{<<"id">> => UserId}, #{projector => #{<<"_id">> => false, <<"password">> => false, <<"hashPassword">> => false}}),
  case (maps:size(Item) =/= 0) of
    true ->
      {ok, from_map(Item)};
    false ->
      {error, <<"Not find user">>}
  end.

get(Email, HashPassword) ->
  Item = em_storage:find_one(<<"users">>, #{<<"email">> => Email, <<"hashPassword">> => HashPassword}, #{projector => #{<<"_id">> => false, <<"password">> => false, <<"hashPassword">> => false}}),
  case (maps:size(Item) =/= 0) of
    true ->
      {ok, from_map(Item)};
    false ->
      {error, <<"Not find user">>}
  end.
