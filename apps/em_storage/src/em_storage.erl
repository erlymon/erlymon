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

-module(em_storage).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% storage: storage library's entry point.

-export([my_func/0]).

-export([
  insert/2
]).

-export([
  update/3,
  update/4,
  update/5
]).

-export([
  delete/2,
  delete_one/2
]).

-export([
  find/2,
  find/3,
  find/4,
  find/5,
  find_one/2,
  find_one/3,
  find_one/4
]).

-export([
  count/2,
  count/3
]).

-export([test/0]).

-define(POOL_NAME, mongo).
%% API
my_func() ->
  ok().


%% Internals

ok() ->
  ok.

-spec insert(mongo:collection(), map() | list()) -> ok.
insert(Coll, Doc) when is_map(Doc) ->
  bson_to_map(poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {insert, Coll, bson:map_to_bson(Doc)})
  end));
insert(Coll, Docs) when is_list(Docs) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {insert, Coll, lists:foldl(fun(Doc, Acc) -> [bson:map_to_bson(Doc)|Acc]  end, [], Docs)})
  end).


-spec update(mongo:collection(), map(), map()) -> ok.
update(Coll, Selector, Doc) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {update, Coll, bson:map_to_bson(Selector), {'$set', bson:map_to_bson(Doc)}})
  end).

-spec update(mongo:collection(), map(), map(), boolean()) -> ok.
update(Coll, Selector, Doc, Upsert) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {update, Coll, bson:map_to_bson(Selector), {'$set', bson:map_to_bson(Doc)}, Upsert})
  end).

-spec update(mongo:collection(), map(), map(), boolean(), boolean()) -> ok.
update(Coll, Selector, Doc, Upsert, MultiUpdate) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {update, Coll, bson:map_to_bson(Selector), {'$set', bson:map_to_bson(Doc)}, Upsert, MultiUpdate})
  end).


-spec delete(bson:collection(), map()) -> ok.
delete(Coll, Selector) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {delete, Coll, bson:map_to_bson(Selector)})
  end).

-spec delete_one(bson:collection(), map()) -> ok.
delete_one(Coll, Selector) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {delete_one, Coll, bson:map_to_bson(Selector)})
  end).


-spec find_one(mongo:collection(), map()) -> map().
find_one(Coll, Selector) ->
    Callback = fun(Worker) ->
      gen_server:call(Worker, {find_one, Coll, bson:map_to_bson(Selector)})
    end,
    case poolboy:transaction(?POOL_NAME, Callback) of
        {} ->
            null;
        {Doc} ->
            bson_to_map(Doc)
    end.

-spec find_one(mongo:collection(), map(), map()) -> map().
find_one(Coll, Selector, Projector) ->
  bson_to_map(poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find_one, Coll, bson:map_to_bson(Selector), bson:map_to_bson(Projector)})
  end)).

-spec find_one(mongo:collection(), map(), map(), mongo:skip()) -> map().
find_one(Coll, Selector, Projector, Skip) ->
  bson_to_map(poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find_one, Coll, bson:map_to_bson(Selector), bson:map_to_bson(Projector), Skip})
  end)).


-spec find(mongo:collection(), map()) -> mongo:cursor().
find(Coll, Selector) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find, Coll, bson:map_to_bson(Selector)})
  end).

-spec find(mongo:collection(), map(), map()) -> mongo:cursor().
find(Coll, Selector, Projector) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find, Coll, bson:map_to_bson(Selector), bson:map_to_bson(Projector)})
  end).

-spec find(mongo:collection(), map(), map(), mongo:skip()) -> mongo:cursor().
find(Coll, Selector, Projector, Skip) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find, Coll, bson:map_to_bson(Selector), bson:map_to_bson(Projector), Skip})
  end).

-spec find(mongo:collection(), map(), map(), mongo:skip(), mongo:batchsize()) -> mongo:cursor().
find(Coll, Selector, Projector, Skip, BatchSize) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find, Coll, bson:map_to_bson(Selector), bson:map_to_bson(Projector), Skip, BatchSize})
  end).


%@doc Count selected documents
-spec count(mongo:collection(), map()) -> integer().
count(Coll, Selector) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {count, Coll, bson:map_to_bson(Selector)})
  end).

%@doc Count selected documents up to given max number; 0 means no max.
%     Ie. stops counting when max is reached to save processing time.
-spec count(mongo:collection(), map(), integer()) -> integer().
count(Coll, Selector, Limit) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {count, Coll, bson:map_to_bson(Selector), Limit})
  end).


bson_to_map(Doc) ->
    bson:doc_foldl(fun(Label, Value, Acc) -> 
                           maps:put(Label, Value, Acc)  
                   end, #{}, Doc).

test() ->
  insert(adf_test, {a,1,b,1}),
  insert(adf_test, [{a,2,b,2}]),
  find_one(adf_test, {}, {'_id', 1, a, 1}, 0).
%% End of Module.
