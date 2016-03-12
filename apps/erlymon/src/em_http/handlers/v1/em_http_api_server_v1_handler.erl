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
-module(em_http_api_server_v1_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").

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
    %%{"success":true,"data":{"zoom":0,"registration":true,"latitude":0.0,"longitude":0.0,"id":1}}
    case em_data_manager:get_server() of
        null ->
            cowboy_req:reply(?STATUS_BAD_REQUEST, Req);
        Server ->
            cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(Server), Req)
    end.

    
-spec update_server(Req :: cowboy_req:req()) -> cowboy_req:req().
update_server(Req) ->
  %%{"success":true,"data":{"zoom":0,"registration":true,"latitude":0.0,"longitude":0.0,"id":1}}
  em_logger:info("SESSION: ~w", [cowboy_session:get(user, Req)]),
  case cowboy_session:get(user, Req) of
    {undefined, Req2} ->
      cowboy_req:reply(?STATUS_BAD_REQUEST, Req2);
    {User, Req2} ->
      em_logger:info("User: ~w", [maps:get(<<"id">>,User)]),
      case em_permissions_manager:check_admin(maps:get(<<"id">>, User)) of
           false ->
              cowboy_req:reply(?STATUS_BAD_REQUEST, Req2);
           _ ->
              {ok, [{JsonBin, true}], Req3} = cowboy_req:body_qs(Req2),
              Server = em_json:decode(JsonBin),
              em_data_manager:update_server(Server),
              cowboy_req:reply(?STATUS_OK, Req3)
      end
  end.
