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
-module(em_google_worker).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%% Url: https://maps.googleapis.com/maps/api/geocode/json?address=Moscow&key=API_KEY
-define(ADDRESS_URL, <<"https://maps.googleapis.com/maps/api/geocode/json?address={address}&language={language}">>).
%% Url: https://maps.googleapis.com/maps/api/geocode/json?latlng=40.714224,-73.961452&key=API_KEY
-define(COORDS_URL, <<"https://maps.googleapis.com/maps/api/geocode/json?latlng={lat},{lon}&language={language}">>).

-record(state, {key}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

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
init(Args) ->
  process_flag(trap_exit, true),
  Key = (proplists:get_value(key, Args)),
  {ok, #state{key = Key}}.

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
handle_call({geocode, Address, Language}, _From, #state{key = Key} = State) ->
  {reply, geocode_address(Key, Address, Language), State};
handle_call({geocode, Latitude, Longitude, Language}, _From, #state{key = Key} = State) ->
  {reply, geocode_coords(Key, Latitude, Longitude, Language), State};
%%handle_call({equery, Stmt, Params}, _From, #state{conn = Conn} = State) ->
%%  {reply, epgsql:equery(Conn, Stmt, Params), State};
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
terminate(_Reason, #state{}) ->
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
geocode_address(Key, Address, Language) ->
  Url = create_url(?ADDRESS_URL, [
    {<<"{key}">>, Key},
    {<<"{address}">>, Address},
    {<<"{language}">>, list_to_binary(atom_to_list(Language))}
  ]),
  em_logger:info("URL: ~s", [Url]),
  case ibrowse:send_req(binary_to_list(Url), [], get) of
    {ok, "200", _Headers, Body} ->
      %logger:debug("BODY: ~w", [list_to_binary(Body)]),
      Res = json:decode(list_to_binary(Body)),
      {ok, Res};
    _ ->
      {error}
  end.

geocode_coords(Key, Latitude, Longitude, Language) ->
  Url = create_url(?COORDS_URL, [
    {<<"{key}">>, Key},
    {<<"{lat}">>, float_to_string(Latitude)},
    {<<"{lon}">>, float_to_string(Longitude)},
    {<<"{language}">>, list_to_binary(atom_to_list(Language))}
  ]),
  em_logger:info("URL: ~s", [Url]),
  case ibrowse:send_req(binary_to_list(Url), [], get) of
    {ok, "200", _Headers, Body} ->
      %logger:debug("BODY: ~w", [list_to_binary(Body)]),
      Object = em_json:decode(list_to_binary(Body)),
      Status = maps:get(<<"status">>, Object),
      Results = maps:get(<<"results">>, Object),
      parse_result(Status, Results);
    _ ->
      {error}
  end.
  
parse_result(<<"OK">>, [#{<<"formatted_address">> := Result}|_]) ->
  {ok, Result};
parse_result(<<"ZERO_RESULTS">>, []) ->
  {error, <<"ZERO_RESULTS">>}.

%% create_url(url, [{key, <<>>}, {address, <<>>}])
%% create_url(url, [{key, <<>>}, {lat, 0.0}, {lon, 0.0}])
create_url(UrlTemplate, Params) ->
  lists:foldr(fun({Key, Value}, Url) ->
    binary:replace(Url, Key, Value)
  end, UrlTemplate, Params).


float_to_string(Value) ->
  list_to_binary(io_lib:format("~f", [Value])).