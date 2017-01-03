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
-module(em_http).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-include("em_records.hrl").

%% API
-export([str/1]).

-spec str(Rec :: term() | list()) -> string().
str(Recs) when is_list(Recs) ->
  em_logger:info("CONVERT RECORDS: ~w", [Recs]),
  em_json:encode(lists:map(fun(Rec) -> rec_to_map(Rec) end, Recs));
str(Rec) ->
  em_json:encode(rec_to_map(Rec)).

rec_to_map(Rec) when is_record(Rec, server) ->
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
  };
rec_to_map(Rec) when is_record(Rec, user)->
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
    <<"password">> => <<"">>
  };
rec_to_map(Rec) when is_record(Rec, device) ->
  {ok, Date} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, Rec#device.lastUpdate),
  #{
    <<"id">> => Rec#device.id,
    <<"name">> => Rec#device.name,
    <<"uniqueId">> => Rec#device.uniqueId,
    <<"status">> => Rec#device.status,
    <<"lastUpdate">> => Date,
    <<"positionId">> => Rec#device.positionId
  };
rec_to_map(Rec) when is_record(Rec, position) ->
  {ok, ServerTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, Rec#position.serverTime),
  {ok, DeviceTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, Rec#position.deviceTime),
  {ok, FixTime} = em_helper_time:format(<<"%Y-%m-%dT%H:%M:%S.000%z">>, Rec#position.fixTime),
  #{
    <<"id">> => Rec#position.id,
    <<"type">> => Rec#position.type,
    <<"protocol">> => Rec#position.protocol,
    <<"serverTime">> => ServerTime,
    <<"deviceTime">> => DeviceTime,
    <<"fixTime">> => FixTime,
    <<"deviceId">> => Rec#position.deviceId,
    <<"outdated">> => Rec#position.outdated,
    <<"valid">> => Rec#position.valid,
    <<"latitude">> => Rec#position.latitude,
    <<"longitude">> => Rec#position.longitude,
    <<"altitude">> => Rec#position.altitude,
    <<"speed">> => Rec#position.speed,
    <<"course">> => Rec#position.course,
    <<"address">> => Rec#position.address,
    <<"attributes">> => Rec#position.attributes
  };
rec_to_map(Rec) when is_record(Rec, event) ->
  #{
    <<"devices">> => lists:map(fun(R) -> rec_to_map(R) end, Rec#event.devices),
    <<"positions">> => lists:map(fun(R) -> rec_to_map(R) end, Rec#event.positions)
  }.