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
-module(em_osmand_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").


%% API
-export([init/2]).
-export([parse_param/2]).

%% http://localhost:5055/?id=123456&lat={0}&lon={1}&timestamp={2}&hdop={3}&altitude={4}&speed={5}
%% id=353490069161244&timestamp=1440085518&lat=53.944019&lon=27.6468195&speed=0.0&bearing=0.0&altitude=0.0&batt=53.0
init(Req, Opts) ->    
    io:format("OPTS: ~w", [Opts]),
    Method = cowboy_req:method(Req),
    %em_logger:debug("URL: ~s", [cowboy_req:url(Req)]),
    %%#{id := Id, lat := Lat, lon := Lon, timestamp := Timestamp, hdop := Hdop, altitude := Alt, speed := Speed, batt := Bat} = cowboy_req:match_qs([id,lat,lon,timestamp,hdop,altitude,speed,batt], Req),
    Map = cowboy_req:match_qs([id,timestamp,lat,lon], Req),
    em_logger:debug("QUERY: ~s", [em_json:encode(Map)]),
    Req2 = echo(Method, Map, Req),
    {ok, Req2, Opts}.

echo(<<"GET">>, #{id := Imei, timestamp := Timestamp, lat := Lat, lon := Lon}, Req) ->
    BaseParams = #{
      deviceTime => parse_time(Timestamp),
      latitude => parse_coord(Lat),
      longitude => parse_coord(Lon),
      valid => true
     },


    OtherParams = lists:foldl(fun(Item, Acc) -> 
                                      try cowboy_req:match_qs([Item], Req) of
                                          Param ->
                                              parse_param(Acc, Param)
                                      catch
                                          error:_ ->
                                              Acc
                                      end
                              end, #{}, [speed, bearing, heading, altitude, hdop, vacc, hacc, batt, desc]),

    Message = maps:merge(BaseParams, OtherParams),
    em_logger:info("[packet] unit:  imei = '~s' message: ~w", [Imei, Message]),
    case em_data_manager:get_device_by_uid(Imei) of
        null ->
            em_logger:info("[packet] unit: unknown device with imei = '~s'", [Imei]),
            cowboy_req:reply(400, [], <<"Permission denide.">>, Req);
        Object ->
            em_data_manager:create_message(maps:get(<<"id">>, Object), protocol(), maps:merge(#{imei => maps:get(<<"uniqueId">>, Object)}, Message)),
            cowboy_req:reply(200, Req)
    end;
echo(<<"GET">>, undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).


parse_param(Map, #{speed := undefined}) ->
    Map;
parse_param(Map, #{speed := Value}) ->
    maps:put(speed, bin_to_num(Value), Map);
parse_param(Map, #{bearing := undefined}) ->
    Map;
parse_param(Map, #{bearing := Value}) ->
    maps:put(bearing, bin_to_num(Value), Map);
parse_param(Map, #{heading := undefined}) ->
    Map;
parse_param(Map, #{heading := Value}) ->
    maps:put(heading, bin_to_num(Value), Map);
parse_param(Map, #{altitude := undefined}) ->
    Map;
parse_param(Map, #{altitude := Value}) ->
    maps:put(altitude, bin_to_num(Value), Map);
parse_param(Map, #{hdop := undefined}) ->
    Map;
parse_param(Map, #{hdop := Value}) ->
    maps:put(hdop, bin_to_num(Value), Map);
parse_param(Map, #{vacc := undefined}) ->
    Map;
parse_param(Map, #{vacc := Value}) ->
    maps:put(vacc, bin_to_num(Value), Map);
parse_param(Map, #{hacc := undefined}) ->
    Map;
parse_param(Map, #{hacc := Value}) ->
    maps:put(hacc, bin_to_num(Value), Map);
parse_param(Map, #{batt := undefined}) ->
    Map;
parse_param(Map, #{batt := Value}) ->
    maps:put(batt, bin_to_num(Value), Map);
parse_param(Map, #{desc := undefined}) ->
    Map;
parse_param(Map, #{desc := Value}) ->
    maps:put(desc, Value, Map);
parse_param(Map, _) ->
    Map.

parse_time(Value) ->
    bin_to_num(Value) * 1000.

parse_coord(Value) ->
    bin_to_num(Value).

bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error, no_float} ->
            list_to_integer(N);
        {F, _Rest} ->
            F
    end.

protocol() ->
    Bin = atom_to_binary(?MODULE, utf8),
    [_, Protocol| _] = binary:split(Bin, <<"_">>, [global]),
      binary_to_atom(Protocol, utf8).
