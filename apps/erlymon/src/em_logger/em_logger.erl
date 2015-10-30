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

info(Formatted) ->
    lager:info(Formatted).

info(Formatted, Args) ->
    lager:info(Formatted, Args).

warning(Formatted) ->
    lager:warning(Formatted).

warning(Formatted, Args) ->
    lager:warning(Formatted, Args).

error(Formatted) ->
    lager:error(Formatted).

error(Formatted, Args) ->
    lager:error(Formatted, Args).

debug(Formatted) ->
    lager:debug(Formatted).

debug(Formatted, Args) ->
    lager:debug(Formatted, Args).


trace_file(Path, Options) ->
    lager:trace_file(Path, Options).

stop_trace(Trace) ->
    lager:stop_trace(Trace).

%% Internals

%% End of Module.
