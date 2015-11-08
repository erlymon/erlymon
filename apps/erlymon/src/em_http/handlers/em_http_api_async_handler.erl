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
-module(em_http_api_async_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").

%% POST http://demo.traccar.org/api/async
%% FORM DATA:
%% first:true
%%
%% ANSWER:
%% {"success":true,"data":[]}
-spec init(Req::cowboy_req:req(), Opts::any()) -> {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    {ok, request(Method, Req), Opts}.

-spec request(Method::binary(), Opts::any()) -> cowboy_req:req().
request(?POST, Req) when (Req /= undefined) ->
    async(Req);
request(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, Req).

-spec async(Req::cowboy_req:req()) -> cowboy_req:req().
async(Req) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{<<"success">> => false}), Req2);
        {User, Req2} ->
	    Devices = em_data_manager:get_devices(maps:get(<<"id">>, User), #{<<"_id">> => false, <<"password">> => false, <<"lastUpdate">> => false}),
	    LastMessages = lists:foldl(fun(Device, Acc) -> 
	      case em_data_manager:get_last_message(maps:get(<<"messageId">>, Device), maps:get(<<"id">>, Device)) of
		null ->
		  Acc;
		Message ->  
		  [Message | Acc]
	      end
	    end, [], Devices),
            cowboy_req:reply(?STATUS_OK, ?HEADERS, em_json:encode(#{<<"success">> => true, <<"data">> => LastMessages}), Req2)
    end.
