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

-module(em_model_user).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-include("em_records.hrl").

%% API
-export([
  to_map/1,
  from_map/1,
  to_str/1,
  create/1,
  update/1,
  delete/1,
  get_all/0,
  get/2,
  get_by_id/1,
  get_by_email/1
]).

to_map(Rec) ->
  #{
    <<"id">> => Rec#user.id,
    <<"name">> => Rec#user.name,
    <<"email">> => Rec#user.email,
    <<"readonly">> => Rec#user.readonly,
    <<"admin">> => Rec#user.admin,
    <<"map">> => Rec#user.map,
    <<"language">> => Rec#user.language,
    <<"distanceUnit">> => Rec#user.distanceUnit,
    <<"speedUnit">> => Rec#user.speedUnit,
    <<"latitude">> => Rec#user.latitude,
    <<"longitude">> => Rec#user.longitude,
    <<"zoom">> => Rec#user.zoom,
    <<"password">> => Rec#user.password,
    <<"hashPassword">> => Rec#user.hashPassword,
    <<"salt">> => Rec#user.salt
  }.

from_map(Map) ->
  Fun = fun(K,V,User) ->
          case K of
            <<"id">> -> User#user{id = V};
            <<"name">> -> User#user{name = V};
            <<"email">> -> User#user{email = V};
            <<"readonly">> -> User#user{readonly = V};
            <<"admin">> -> User#user{admin = V};
            <<"map">> -> User#user{map = V};
            <<"language">> -> User#user{language = V};
            <<"distanceUnit">> -> User#user{distanceUnit = V};
            <<"speedUnit">> -> User#user{speedUnit = V};
            <<"latitude">> -> User#user{latitude = V};
            <<"longitude">> -> User#user{longitude = V};
            <<"zoom">> -> User#user{zoom = V};
            <<"password">> -> User#user{password = V};
            <<"hashPassword">> -> User#user{hashPassword = V};
            <<"salt">> -> User#user{salt = V};
            _ ->
                User
          end
        end,
  maps:fold(Fun,#user{},Map).

to_str(Recs) when is_list(Recs) ->
  em_logger:info("CONVERT RECORDS: ~w", [Recs]),
  em_json:encode(lists:map(fun(Rec) -> to_map(Rec) end, Recs));
to_str(Rec) ->
  em_json:encode(to_map(Rec)).


create(Rec) ->
  User = Rec#user{
    id = em_helper_time:timestamp() div 1000000,
    hashPassword = em_password:hash(Rec#user.password)
  },
  {_, Item} = em_storage:insert(<<"users">>, to_map(User)),
  {ok, from_map(Item)}.

update(Rec) ->
  FixUser = fun(UserModel = #user{password = Password}) ->
                case Password of
                  undefinded ->
                    UserModel0 = maps:remove(<<"password">>, to_map(UserModel)),
                    UserModel1 = maps:remove(<<"hashPassword">>, UserModel0);
                  <<"">> ->
                    UserModel0 = maps:remove(<<"password">>, to_map(UserModel)),
                    UserModel1 = maps:remove(<<"hashPassword">>, UserModel0);
                  _ ->
                    UserModel1 = to_map(UserModel#user{hashPassword = em_password:hash(Password)})
                end,
                maps:remove(<<"id">>, UserModel1)
              end,
  em_storage:update(<<"users">>, #{<<"id">> => Rec#user.id}, FixUser(Rec)).

delete(Rec) ->
  em_storage:delete_one(<<"users">>, #{<<"id">> => Rec#user.id}).

get_all() ->
  Callback = fun(User) ->
              from_map(User)
             end,
  Cursor = em_storage:find(<<"users">>, #{}, #{projector => #{<<"_id">> => false, <<"password">> => false, <<"hashPassword">> => false}}),
  Users = em_storage_cursor:map(Callback, Cursor),
  em_storage_cursor:close(Cursor),
  {ok, Users}.

get_by_id(UserId) ->
  Item = em_storage:find_one(<<"users">>, #{<<"id">> => UserId}, #{projector => #{<<"_id">> => false, <<"password">> => false, <<"hashPassword">> => false}}),
  case (maps:size(Item) =/= 0) of
    true ->
      {ok, from_map(Item)};
    false ->
      {error, <<"Not find user">>}
  end.

get_by_email(Email) ->
  Item = em_storage:find_one(<<"users">>, #{<<"email">> => Email}, #{projector => #{<<"_id">> => false, <<"password">> => false, <<"hashPassword">> => false}}),
  case (maps:size(Item) =/= 0) of
    true ->
      {ok, from_map(Item)};
    false ->
      {error, <<"Not find user">>}
  end.

get(Email, HashPassword) ->
  Item = em_storage:find_one(<<"users">>, #{<<"email">> => Email, <<"hashPassword">> => HashPassword}, #{projector => #{<<"_id">> => false, <<"password">> => false, <<"hashPassword">> => false}}),
  case (maps:size(Item) =/= 0) of
    true ->
      {ok, from_map(Item)};
    false ->
      {error, <<"Not find user">>}
  end.
