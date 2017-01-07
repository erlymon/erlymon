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
-module(em_hardware).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-export([
  start/1,
  stop/1
]).

-define(ACCEPTORS, 10).

-spec start(Args :: any()) -> any().
start(HwArgs) ->
  lists:foreach(fun({Protocol, Options, Args}) ->
    em_logger:info("Start hardware server  ~w port: ~w",[Protocol, proplists:get_value(port, Options)]),
    case Protocol of
      osmand ->
        Dispatch = cowboy_router:compile([{'_', [{"/", em_osmand_protocol, []}]}]),
        {ok, _} = cowboy:start_http(Protocol, 100, Options, [{env, [{dispatch, Dispatch}]}]);
      _ ->
        {ok, _} = ranch:start_listener(Protocol, ?ACCEPTORS, ranch_tcp, Options, make_module_name(Protocol), [{protocol, Protocol}, Args])
    end
                end, HwArgs).

-spec stop(Args :: any()) -> any().
stop(Args) ->
  lists:foreach(fun({Protocol, _, _}) ->
    case Protocol of
      osmand ->
        cowboy:stop_listener(Protocol);
      _ ->
        ranch:stop_listener(Protocol)
    end
                end, Args),
  ok.

make_module_name(Name) ->
  list_to_atom("em_" ++ atom_to_list(Name) ++ "_protocol").
%% End of Module.
