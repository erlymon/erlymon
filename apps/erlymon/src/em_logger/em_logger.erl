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

-module(em_logger).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% logger: logger library's entry point.

-export([info/1]).
-export([info/2]).

-export([warning/1]).
-export([warning/2]).

-export([error/1]).
-export([error/2]).

-export([debug/1]).
-export([debug/2]).


-export([trace_file/2]).
-export([stop_trace/1]).
%% API

-spec(info(Formatted :: string()) -> ok).
info(Formatted) ->
    lager:info(Formatted).

-spec(info(Formatted :: string(), Args :: list()) -> ok).
info(Formatted, Args) ->
    lager:info(Formatted, Args).

-spec(warning(Formatted :: string()) -> ok).
warning(Formatted) ->
    lager:warning(Formatted).

-spec(warning(Formatted :: string(), Args :: list()) -> ok).
warning(Formatted, Args) ->
    lager:warning(Formatted, Args).

-spec(error(Formatted :: string()) -> ok).
error(Formatted) ->
    lager:error(Formatted).

-spec(error(Formatted :: string(), Args :: list()) -> ok).
error(Formatted, Args) ->
    lager:error(Formatted, Args).

-spec(debug(Formatted :: string()) -> ok).
debug(Formatted) ->
    lager:debug(Formatted).

-spec(debug(Formatted :: string(), Args :: list()) -> ok).
debug(Formatted, Args) ->
    lager:debug(Formatted, Args).

-spec(trace_file(Formatted :: string(), Args :: list()) -> ok).
trace_file(Path, Options) ->
    lager:trace_file(Path, Options).

-spec(stop_trace(Formatted :: string()) -> ok).
stop_trace(Trace) ->
    lager:stop_trace(Trace).

%% Internals

%% End of Module.
