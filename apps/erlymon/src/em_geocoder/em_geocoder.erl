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

-behaviour(gen_server).

-include("em_records.hrl").

%% API
-export([start_link/2]).
-export([reverse/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          type :: atom(),
          cache
         }).

%%%===================================================================
%%% API
%%%===================================================================

-spec(reverse(Latitude :: float(), Longitude :: float(), Language :: string()) -> {ok, string()} | {error, string()}).
reverse(Latitude, Longitude, Language) ->
    gen_server:call(?SERVER, {reverse, Latitude, Longitude, Language}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(GeocoderType :: atom(), GeocoderSettings :: list()) ->
             {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(GeocoderType, GeocoderSettings) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GeocoderType, GeocoderSettings], []).

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
init([GeocoderType, _GeocoderSettings]) ->
    em_logger:info("Init '~s' geocoder service", [GeocoderType]),
    Cache = ets:new(address, [set, private]),
    {ok, #state{type = GeocoderType, cache = Cache}}.
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
terminate(_Reason, #state{cache = Cache}) ->
    ets:delete(Cache),
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
do_reverse(State = #state{type = Type, cache = Cache}, Latitude, Longitude, Language) ->
  case ets:lookup(Cache, {Latitude, Longitude}) of
    [] ->
      case do_reverse_provider(Type, Latitude, Longitude, Language) of
        {ok, Address} ->
          case ets:insert_new(Cache, {{Latitude, Longitude}, Address}) of
            true ->
              {reply, {ok, do_address_format(Address)}, State};
            false ->
              {reply, {error, <<"Error sync in address cache">>}, State}
          end;
        Reason ->
          {reply, Reason, State}
      end;
    [{_, Address} | _] ->
      {reply, {ok, do_address_format(Address)}, State}
  end.

do_reverse_provider(google, Latitude, Longitude, Language) ->
    em_geocoder_google:reverse(Latitude, Longitude, Language);
do_reverse_provider(yandex, Latitude, Longitude, Language) ->
    em_geocoder_yandex:reverse(Latitude, Longitude, Language);
do_reverse_provider(nominatim, Latitude, Longitude, Language) ->
    em_geocoder_nominatim:reverse(Latitude, Longitude, Language);
do_reverse_provider(_, _, _, _) ->
    {error, <<"Unknown provider">>}.


%% Available parameters:
%% %p - postcode
%% %c - country
%% %s - state
%% %d - district
%% %t - settlement (town)
%% %u - suburb
%% %r - street (road)
%% %h - house
do_address_format(Address) ->
    do_address_format("%h %r, %t, %s, %c", Address).

do_address_format(Format, Address) ->
    Fields = [
              {"%p", Address#address.postcode},
              {"%c", Address#address.country},
              {"%s", Address#address.state},
              {"%d", Address#address.country},
              {"%t", Address#address.settlement},
              {"%u", Address#address.suburb},
              {"%r", Address#address.street},
              {"%h", Address#address.house}
             ],
    Result = lists:foldl(fun({Key, Value}, Acc) ->
                                 do_replace(Acc, Key, Value)
                         end, Format, Fields),
    re:replace(Result, "^[, ]*", "", [{return, binary}, global]).

do_replace(S, K, undefined) ->
    re:replace(S, "[, ]*" ++ K, "", [{return, list}, global]);
do_replace(S, K, V) ->
    re:replace(S, K, V, [{return, list}, global]).