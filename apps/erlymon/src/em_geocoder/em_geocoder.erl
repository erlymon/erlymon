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
-module(em_geocoder).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").


%% geocoder: geocoder library's entry point.

-export([
    geocode/2,
    geocode/3
]).


%% API

geocode(Address, Language) ->
    poolboy:transaction(google, fun(Worker) ->
        gen_server:call(Worker, {geocode, Address, Language})
    end).

geocode(Latitude, Longitude, Language) ->
    poolboy:transaction(google, fun(Worker) ->
        gen_server:call(Worker, {geocode, Latitude, Longitude, Language})
    end).
%% Internals


%% End of Module.
