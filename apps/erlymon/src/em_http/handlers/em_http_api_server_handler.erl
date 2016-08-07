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
-module(em_http_api_server_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").
-include("em_records.hrl").

%% GET http://demo.traccar.org/api/server/get?_dc=1436250979884
%% QUERY STRING PARAM:
%% _dc:1436250979884

%% ANSWER:
%% {"id":1,"registration":true,"readonly":false,"map":null,"bingKey":null,"mapUrl":null,"language":null,"distanceUnit":null,"speedUnit":null,"latitude":0.0,"longitude":0.0,"zoom":0}

-spec init(Req::cowboy_req:req(), Opts::any()) -> {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
  Method = cowboy_req:method(Req),
      {ok, request(Method, Req), Opts}.

-spec request(Method::binary(), Opts::any()) -> cowboy_req:req().
request(?GET, Req) ->
    get_server(Req);
request(?PUT, Req) ->
    update_server(Req);
request(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, Req).

-spec get_server(Req::cowboy_req:req()) -> cowboy_req:req().
get_server(Req) ->
  {ok, Server} = em_data_manager:get_server(),
  cowboy_req:reply(?STATUS_OK, ?HEADERS, em_model_server:to_str(Server), Req).

    
-spec update_server(Req :: cowboy_req:req()) -> cowboy_req:req().
update_server(Req) ->
  {ok, [{JsonBin, true}], Req1} = cowboy_req:body_qs(Req),
  em_logger:info("SERVER JSON: ~s", [JsonBin]),
  Result = emodel:from_map(em_json:decode(JsonBin), #server{}, [
    {<<"id">>, required, integer, #server.id, []},
    {<<"registration">>, required, boolean, #server.registration, []},
    {<<"readonly">>, required, boolean, #server.readonly, []},
    {<<"map">>, required, string, #server.map, []},
    {<<"bingKey">>, required, string, #server.bingKey, []},
    {<<"mapUrl">>, required, string, #server.mapUrl, []},
    {<<"language">>, required, string, #server.language, []},
    {<<"distanceUnit">>, required, string, #server.distanceUnit, []},
    {<<"speedUnit">>, required, string, #server.speedUnit, []},
    {<<"latitude">>, required, integer, #server.latitude, []},
    {<<"longitude">>, required, integer, #server.longitude, []},
    {<<"zoom">>, required, integer, #server.zoom, []}
  ]),
  em_logger:info("Result: ~w", [Result]),
  case Result of
    {ok, Server} ->
      case cowboy_session:get(user, Req1) of
        {undefined, Req2} ->
          cowboy_req:reply(?STATUS_BAD_REQUEST, Req2);
        {User, Req2} ->
          em_logger:info("User: ~w", [maps:get(<<"id">>,User)]),
          case em_permissions_manager:check_admin(maps:get(<<"id">>, User)) of
            false ->
              cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req2);
            _ ->
              em_data_manager:update_server(Server),
              cowboy_req:reply(?STATUS_OK, ?HEADERS, em_model_server:to_str(Server), Req2)
          end
      end;
    {error, Reason} ->
      %% Encode end set error
      cowboy_req:reply(?STATUS_UNKNOWN, [], em_json:encode(Reason), Req1)
  end.
