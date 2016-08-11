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
-module(em_http_api_users_handler).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

%% API

-export([init/2]).

-include("em_http.hrl").
-include("em_records.hrl").

%% GET http://demo.traccar.org/api/user/get?_dc=1436251203853&page=1&start=0&limit=25
%% {"success":true,"data":[]}
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  case cowboy_session:get(user, Req) of
    {undefined, Req2} ->
      {ok, request(Method, Req2), Opts};
    {User, Req2} ->
      {ok, request(Method, Req2, User), Opts}
  end.

-spec request(Method :: binary(), Req :: cowboy_req:req(), User :: map()) -> cowboy_req:req().
request(?GET, Req, User) ->
  get_users(Req, User);
request(?POST, Req, User) ->
  add_user(Req, User);
request(?PUT, Req, User) ->
  update_user(Req, User);
request(?DELETE, Req, User) ->
  remove_user(Req, User);
request(_, Req, _) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, [], <<"Allowed GET,POST,PUT,DELETE requests.">>, Req).


-spec request(Method :: binary(), Req :: cowboy_req:req()) -> cowboy_req:req().
request(?POST, Req) ->
  add_user(Req);
request(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(?STATUS_METHOD_NOT_ALLOWED, [], <<"Allowed POST request.">>, Req).

-spec get_users(Req :: cowboy_req:req(), User :: map()) -> cowboy_req:req().
get_users(Req, User) ->
  em_logger:info("User: ~w", [User#user.id]),
  case em_permissions_manager:check_admin(User#user.id) of
    false ->
      cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req);
    _ ->
      {ok, Users} = em_data_manager:get_users(),
      cowboy_req:reply(?STATUS_OK, ?HEADERS, em_model_user:to_str(Users), Req)
  end.

-spec add_user(Req :: cowboy_req:req()) -> cowboy_req:req().
add_user(Req) ->
  {ok, [{JsonBin, true}], Req2} = cowboy_req:body_qs(Req),
  em_logger:info("REGISTRY USER JSON: ~s", [JsonBin]),
  Result = emodel:from_map(em_json:decode(JsonBin), #user{}, [
    {<<"name">>, required, string, #user.name, []},
    {<<"email">>, required, string, #user.email, []},
    {<<"password">>, required, string, #user.password, []}
  ]),
  case Result of
    {ok, UserModel} ->
        case em_permissions_manager:check_registration() of
          false ->
            cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req2);
          _ ->
            case em_data_manager:create_user(UserModel) of
              {error, _Reason} ->
                cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req2);
              {ok, User} ->
                cowboy_req:reply(?STATUS_OK, ?HEADERS, em_model_user:to_str(User), Req2)
            end
        end;
    _Reason ->
      cowboy_req:reply(?STATUS_UNKNOWN, [], <<"Invalid format">>, Req)
  end.

-spec add_user(Req :: cowboy_req:req(), User :: map()) -> cowboy_req:req().
add_user(Req, User) ->
  {ok, [{JsonBin, true}], Req2} = cowboy_req:body_qs(Req),
  em_logger:info("ADD USER JSON: ~s", [JsonBin]),
  Result = emodel:from_map(em_json:decode(JsonBin), #user{}, [
    {<<"id">>, required, integer, #user.id, []},
    {<<"name">>, required, string, #user.name, []},
    {<<"email">>, required, string, #user.email, []},
    {<<"password">>, optional, string, #user.password, []},
    {<<"admin">>, required, boolean, #user.admin, []},
    {<<"map">>, required, string, #user.map, []},
    {<<"distanceUnit">>, required, string, #user.distanceUnit, []},
    {<<"speedUnit">>, required, string, #user.speedUnit, []},
    {<<"latitude">>, required, integer, #user.latitude, []},
    {<<"longitude">>, required, integer, #user.longitude, []},
    {<<"zoom">>, required, integer, #user.zoom, []}
  ]),
  case Result of
    {ok, UserModel} ->
          case em_permissions_manager:check_admin(User#user.id) of
            false ->
              cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req2);
            _ ->
              case em_data_manager:create_user(UserModel) of
                {error, _Reason} ->
                  cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req2);
                {ok, NewUser} ->
                  cowboy_req:reply(?STATUS_OK, ?HEADERS, em_model_user:to_str(NewUser), Req2)
              end
          end;
    _Reason ->
      cowboy_req:reply(?STATUS_UNKNOWN, [], <<"Invalid format">>, Req)
  end.

-spec remove_user(Req :: cowboy_req:req(), User :: map()) -> cowboy_req:req().
remove_user(Req, User) ->
  {ok, [{JsonBin, true}], Req2} = cowboy_req:body_qs(Req),
  %%UserModel = em_json:decode(JsonBin),
  em_logger:info("REMOVE USER JSON: ~s", [JsonBin]),
  Result = emodel:from_map(em_json:decode(JsonBin), #user{}, [
    {<<"id">>, required, integer, #user.id, []},
    {<<"name">>, required, string, #user.name, []},
    {<<"email">>, required, string, #user.email, []},
    {<<"password">>, optional, string, #user.password, []},
    {<<"admin">>, required, boolean, #user.admin, []},
    {<<"map">>, required, string, #user.map, []},
    {<<"distanceUnit">>, required, string, #user.distanceUnit, []},
    {<<"speedUnit">>, required, string, #user.speedUnit, []},
    {<<"latitude">>, required, integer, #user.latitude, []},
    {<<"longitude">>, required, integer, #user.longitude, []},
    {<<"zoom">>, required, integer, #user.zoom, []}
  ]),
  case Result of
    {ok, UserModel} ->
            case em_permissions_manager:check_admin(User#user.id) of
              false ->
                cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req);
              _ ->
                em_data_manager:delete_user(UserModel),
                cowboy_req:reply(?STATUS_OK, ?HEADERS, em_model_user:to_str(UserModel), Req2)
            end;
    _Reason ->
      cowboy_req:reply(?STATUS_UNKNOWN, [], <<"Invalid format">>, Req)
  end.

-spec update_user(Req :: cowboy_req:req(), User :: map()) -> cowboy_req:req().
update_user(Req, CurrUser) ->
  {ok, [{JsonBin, true}], Req2} = cowboy_req:body_qs(Req),
  em_logger:info("UPDATE USER JSON: ~s", [JsonBin]),
  Result = emodel:from_map(em_json:decode(JsonBin), #user{}, [
    {<<"id">>, required, integer, #user.id, []},
    {<<"name">>, required, string, #user.name, []},
    {<<"email">>, required, string, #user.email, []},
    {<<"password">>, optional, string, #user.password, []},
    {<<"admin">>, required, boolean, #user.admin, []},
    {<<"map">>, required, string, #user.map, []},
    {<<"distanceUnit">>, required, string, #user.distanceUnit, []},
    {<<"speedUnit">>, required, string, #user.speedUnit, []},
    {<<"latitude">>, required, integer, #user.latitude, []},
    {<<"longitude">>, required, integer, #user.longitude, []},
    {<<"zoom">>, required, integer, #user.zoom, []}
  ]),
  case Result of
    {ok, UserModel} ->
      case check_permission(CurrUser#user.id, UserModel#user.id) of
        false ->
          cowboy_req:reply(?STATUS_FORBIDDEN, [], <<"Admin access required">>, Req2);
        _ ->
          em_data_manager:update_user(UserModel),
          cowboy_req:reply(?STATUS_OK, ?HEADERS, em_model_user:to_str(UserModel), Req2)
      end;
    _Reason ->
      cowboy_req:reply(?STATUS_UNKNOWN, [], <<"Invalid format">>, Req)
  end.



check_permission(CurrUserId, UpdateUserId) when CurrUserId == UpdateUserId ->
  true;
check_permission(CurrUserId, _) ->
  em_permissions_manager:check_admin(CurrUserId).
