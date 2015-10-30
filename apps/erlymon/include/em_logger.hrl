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
-ifndef(LOGGER_HRL).
-define(LOGGER_HRL, true).

-define(INFO_MSG(Format, Data), em_logger:info("~p{~p}: ~s~n", [?MODULE, ?LINE, io_lib:format(Format, Data)])).
-define(WARNING_MSG(Format, Data), em_logger:warning("~p{~p}: ~s~n", [?MODULE, ?LINE, io_lib:format(Format, Data)])).
-define(ERROR_MSG(Format, Data), em_logger:error("~p{~p}: ~s~n", [?MODULE, ?LINE, io_lib:format(Format, Data)])).


-define(HOST(Socket),
  case inet:peername(Socket) of
    {ok, {Address, _Port}} ->
      list_to_binary(io_lib:format("~w.~w.~w.~w", tuple_to_list(Address)));
    {error, _} ->
      <<"unknown">>
  end).

-endif. % LOGGER_HRL
