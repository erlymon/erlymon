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
-module(em_http_api_socket_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API
-include("em_http.hrl").
-include("em_records.hrl").

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

-spec init(Req :: cowboy_req:req(), Opts :: list()) -> {atom(), cowboy_req:req(), list()}.
init(Req, Opts) ->
    case cowboy_session:get(user, Req) of
        {undefined, Req2} ->
            {cowboy_websocket, Req2, Opts};
        {User, Req2} ->
            em_logger:info("Process reg for user: ~w", [User#user.id]),
            em_proc:registry(User#user.id, self()),
            erlang:start_timer(1000, self(), User),
            {cowboy_websocket, Req2, Opts}
    end.

-spec websocket_handle(Data:: any(), Req :: cowboy_req:req(), State :: term()) ->
    {ok, cowboy_req:req(), term()}.
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

-spec websocket_info(term(), Req :: cowboy_req:req(), State :: term()) ->
    {reply, term(), cowboy_req:req(), term()}.
websocket_info({position, Position}, Req, State) ->
    em_logger:info("WS SEND POSITION: ~w", [Position]),
    Language = cowboy_req:header(<<"Accept-Language">>, Req, <<"en_US">>),
    case em_geocoder:reverse(Position#position.latitude, Position#position.longitude, Language) of
        {ok, Address} ->
            {reply, {text, em_http_utils:str(#event{positions = [Position#position{address = Address}]})}, Req, State};
        _ ->
            {reply, {text, em_http_utils:str(#event{positions = [Position]})}, Req, State}
    end;
websocket_info({device, Device}, Req, State) ->
    %%em_logger:info("WS SEND DEVICE: ~w", [Device]),
    {reply, {text, em_http_utils:str(#event{devices = [Device]})}, Req, State};
websocket_info({timeout, _Ref, User}, Req, State) ->
    %%em_logger:info("WS INIT: ~w", [User]),
    Language = cowboy_req:header(<<"Accept-Language">>, Req, <<"en_US">>),
    {ok, Devices} = em_data_manager:get_devices(User#user.id),
    GetLastPosition = fun(Device, Acc) ->
                              case em_data_manager:get_last_position(Device#device.positionId, Device#device.id) of
                                  {error, _Reason} ->
                                      Acc;
                                  {ok, Position} ->
                                      case em_geocoder:reverse(Position#position.latitude, Position#position.longitude, Language) of
                                          {ok, Address} -> [Position#position{address = Address} | Acc];
                                          _ -> [Position | Acc]
                                      end
                              end
                      end,
    Positions = lists:foldl(GetLastPosition, [], Devices),
    {reply, {text, em_http_utils:str(#event{positions = Positions})}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.