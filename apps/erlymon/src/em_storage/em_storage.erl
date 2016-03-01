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
  count/3,
  ensure_index/2
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
  Callback = fun(Worker) ->
    gen_server:call(Worker, {insert, Coll, Doc})
  end,
  poolboy:transaction(?POOL_NAME, Callback);
insert(Coll, Docs) when is_list(Docs) ->
  Callback = fun(Worker) ->
    gen_server:call(Worker, {insert, Coll, Docs})
  end,
  poolboy:transaction(?POOL_NAME, Callback).


-spec update(mongo:collection(), map(), map()) -> ok.
update(Coll, Selector, Doc) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {update, Coll, Selector, #{<<"$set">> => bson:flatten_map(Doc)}})
  end).

-spec update(mongo:collection(), map(), map(), boolean()) -> ok.
update(Coll, Selector, Doc, Upsert) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {update, Coll, Selector, #{<<"$set">> => bson:flatten_map(Doc)}, Upsert})
  end).

-spec update(mongo:collection(), map(), map(), boolean(), boolean()) -> ok.
update(Coll, Selector, Doc, Upsert, MultiUpdate) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {update, Coll, Selector, #{<<"$set">> => bson:flatten_map(Doc)}, Upsert, MultiUpdate})
  end).


-spec delete(bson:collection(), map()) -> ok.
delete(Coll, Selector) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {delete, Coll, Selector})
  end).

-spec delete_one(bson:collection(), map()) -> ok.
delete_one(Coll, Selector) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {delete_one, Coll, Selector})
  end).


-spec find_one(mongo:collection(), map()) -> map().
find_one(Coll, Selector) ->
    Callback = fun(Worker) ->
      gen_server:call(Worker, {find_one, Coll, Selector})
    end,
    poolboy:transaction(?POOL_NAME, Callback).
    %case poolboy:transaction(?POOL_NAME, Callback) of
    %    #{} ->
    %        null;
    %    Doc ->
    %        Doc
    %end.

-spec find_one(mongo:collection(), map(), map()) -> map().
find_one(Coll, Selector, Projector) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find_one, Coll, Selector, Projector})
  end).

-spec find_one(mongo:collection(), map(), map(), mongo:skip()) -> map().
find_one(Coll, Selector, Projector, Skip) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find_one, Coll, Selector, Projector, Skip})
  end).


-spec find(mongo:collection(), map()) -> mongo:cursor().
find(Coll, Selector) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find, Coll, Selector})
  end).

-spec find(mongo:collection(), map(), map()) -> mongo:cursor().
find(Coll, Selector, Projector) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find, Coll, Selector, Projector})
  end).

-spec find(mongo:collection(), map(), map(), mongo:skip()) -> mongo:cursor().
find(Coll, Selector, Projector, Skip) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find, Coll, Selector, Projector, Skip})
  end).

-spec find(mongo:collection(), map(), map(), mongo:skip(), mongo:batchsize()) -> mongo:cursor().
find(Coll, Selector, Projector, Skip, BatchSize) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {find, Coll, Selector, Projector, Skip, BatchSize})
  end).


%@doc Count selected documents
-spec count(mongo:collection(), map()) -> integer().
count(Coll, Selector) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {count, Coll, Selector})
  end).

%@doc Count selected documents up to given max number; 0 means no max.
%     Ie. stops counting when max is reached to save processing time.
-spec count(mongo:collection(), map(), integer()) -> integer().
count(Coll, Selector, Limit) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {count, Coll, Selector, Limit})
  end).

%% @doc Create index on collection according to given spec.
%%      The key specification is a bson documents with the following fields:
%%      key      :: bson document, for e.g. {field, 1, other, -1, location, 2d}, <strong>required</strong>
%%      name     :: bson:utf8()
%%      unique   :: boolean()
%%      dropDups :: boolean()
-spec ensure_index(mongo:collection(), tuple()) -> any().
ensure_index(Coll, Spec) ->
  poolboy:transaction(?POOL_NAME, fun(Worker) ->
    gen_server:call(Worker, {ensure_index, Coll, Spec})
  end).

test() ->
  insert(adf_test, {a,1,b,1}),
  insert(adf_test, [{a,2,b,2}]),
  find_one(adf_test, {}, {'_id', 1, a, 1}, 0).
%% End of Module.
