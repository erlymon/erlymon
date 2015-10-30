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

-module(em_storage_cursor).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-export([next/1, close/1]).

-spec next(mongo:cursor()) -> map().
next(Cursor) ->
    case mc_cursor:next(Cursor) of
      {} ->
	null;
      {Item} ->
	case (maps:size(Item) =/= 0) of
	  true ->
	    Item;
	  false ->
	    null
	end
    end.

-spec close(mongo:cursor()) -> ok.
close(Cursor) ->
    mc_cursor:close(Cursor).


bson_to_map(Doc) ->
    %%io:format("~w~n", [Doc]),
    bson:doc_foldl(fun(Label, Value, Acc) -> 
                           maps:put(Label, Value, Acc)  
                   end, #{}, Doc).
