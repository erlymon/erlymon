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
-module(em_geocoder_google).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(gen_server).

-include("em_records.hrl").

%% API
-export([start_link/0]).
-export([reverse/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(NAME, "google").
-define(HOST, "maps.googleapis.com").
-define(PORT, 443).
-define(REVERSE, <<"/maps/api/geocode/json?latlng={lat},{lon}&language={lang}">>).


-record(state, {conn :: any()}).

%%%===================================================================
%%% API
%%%===================================================================

%% example:
%% error: em_geocoder:reverse(0.0,0.0,<<"ru">>).
%% ok: em_geocoder:reverse(57.234,24.24234,<<"en">>).
-spec(reverse(Latitude :: float(), Longitude :: float(), Language :: string()) -> {ok, string()} | {error, string()}).
reverse(Latitude, Longitude, Language) ->
    gen_server:call(?SERVER, {reverse, Latitude, Longitude, Language}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    em_logger:info("Init '~s' geocoder service", [?NAME]),
    case shotgun:open(?HOST, ?PORT, https) of
        {ok, Conn} ->
            {ok, #state{conn = Conn}};
        Reason ->
            {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({reverse, Latitude, Longitude, Language}, _From, State) ->
    do_reverse(State, Latitude, Longitude, Language);
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, #state{conn = Conn}) ->
    shotgun:close(Conn),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_reverse(State = #state{conn = Conn}, Latitude, Longitude, Language) ->
    Headers = #{},
    URL = do_create_url(?REVERSE, [
        {<<"{lat}">>, erlang:float_to_binary(Latitude,[{decimals, 8}])},
        {<<"{lon}">>, erlang:float_to_binary(Longitude,[{decimals, 8}])},
        {<<"{lang}">>, Language}
    ]),
    em_logger:info("URL: ~s", [URL]),
    case shotgun:get(Conn, URL, Headers) of
        {ok, Response} ->
            case Response of
                #{status_code := 200, body := Body} ->
                    {reply, do_parse_body(jsx:decode(Body, [return_maps])), State};
                #{body := Body} ->
                    {reply, {error, Body}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

do_parse_body(#{<<"error_message">> := Message}) ->
    {error, Message};
do_parse_body(#{<<"status">> := <<"ZERO_RESULTS">>, <<"results">> := []}) ->
    {error, <<"Zerro results">>};
do_parse_body(#{<<"status">> := <<"OK">>, <<"results">> := [#{<<"address_components">> := AddressComponents}|_]}) ->
    Fun = fun(#{<<"short_name">> := ShortName, <<"types">> := Types}, Acc) ->
        do_create_address(ShortName, Types, Acc)
          end,
    {ok, lists:foldl(Fun, #address{}, AddressComponents)}.


do_create_address(_, [], Address) ->
    Address;
do_create_address(Value, [<<"street_number">>|_], Address) ->
    do_create_address(Value, [], Address#address{house = Value});
do_create_address(Value, [<<"route">>|_], Address) ->
    do_create_address(Value, [], Address#address{street = Value});
do_create_address(Value, [<<"locality">>|_], Address) ->
    do_create_address(Value, [], Address#address{settlement = Value});
do_create_address(Value, [<<"administrative_area_level_2">>|_], Address) ->
    do_create_address(Value, [], Address#address{district = Value});
do_create_address(Value, [<<"administrative_area_level_1">>|_], Address) ->
    do_create_address(Value, [], Address#address{state = Value});
do_create_address(Value, [<<"country">>|_], Address) ->
    do_create_address(Value, [], Address#address{country = Value});
do_create_address(Value, [<<"postal_code">>|_], Address) ->
    do_create_address(Value, [], Address#address{postcode = Value});
do_create_address(Value, [_|Tail], Address) ->
    do_create_address(Value, Tail, Address).

do_create_url(UrlTemplate, Params) ->
    lists:foldr(fun({Key, Value}, Url) ->
        binary:replace(Url, Key, Value)
                end, UrlTemplate, Params).