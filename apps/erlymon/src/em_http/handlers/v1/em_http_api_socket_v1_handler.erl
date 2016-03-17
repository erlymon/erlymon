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
-module(em_http_api_socket_v1_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API
-include("em_http.hrl").

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).


%% http://blog.dberg.org/2012/04/using-gproc-and-cowboy-to-pass-messages.html
%% http://ninenines.eu/docs/en/cowboy/HEAD/guide/ws_handlers/

init(Req, Opts) ->
  case cowboy_session:get(user, Req) of
    {undefined, Req2} ->
      {cowboy_websocket, Req2, Opts};
    {User, Req2} ->
      erlang:start_timer(1000, self(), User),
      {cowboy_websocket, Req2, Opts}
  end.

websocket_handle({text, Msg}, Req, State) ->
  {reply, {text, <<"That's what she said! ", Msg/binary>>}, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, User}, Req, State) ->
  Devices = em_data_manager:get_devices(maps:get(<<"id">>, User), #{<<"_id">> => false, <<"password">> => false, <<"lastUpdate">> => false}),
  LastMessages = lists:foldl(fun(Device, Acc) ->
    case em_data_manager:get_last_message(maps:get(<<"positionId">>, Device), maps:get(<<"id">>, Device)) of
      null ->
        Acc;
      Message ->
        [Message | Acc]
    end
                             end, [], Devices),
  erlang:start_timer(1000, self(), User),

  DataModel = #{
    <<"devices">> => Devices,
    <<"positions">> => LastMessages
  },
  {reply, {text, em_json:encode(DataModel)}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.
