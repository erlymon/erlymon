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
-module(em_geocoder_yandex).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(gen_server).

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

-define(NAME, "yandex").
-define(HOST, "geocode-maps.yandex.ru").
-define(PORT, 443).
-define(REVERSE, <<"/1.x/?geocode={lon},{lat}&format=json&results=1&lang={lang}">>).


-record(state, {conn :: any()}).

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
    Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
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
                    em_logger:info("Body: ~s", [Body]),
                    do_parse_body(State, jsx:decode(Body, [return_maps]));
                #{body := Body} ->
                    {reply, {error, Body}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

do_parse_body(State, #{<<"error">> := #{<<"message">> := Message}}) ->
    {reply, {error, Message}, State};
%% {"response":{"GeoObjectCollection":{"metaDataProperty":{"GeocoderResponseMetaData":{"request":"37.6155600,55.7522200","found":"8","results":"1","Point":{"pos":"37.615560 55.752220"}}},"featureMember":[{"GeoObject":{"metaDataProperty":{"GeocoderMetaData":{"kind":"street","text":"Russian Federation, Moscow, Moscow Kremlin, Troitskaya Street","precision":"street","AddressDetails":{"Country":{"AddressLine":"Moscow, Moscow Kremlin, Troitskaya Street","CountryNameCode":"RU","CountryName":"Russian Federation","AdministrativeArea":{"AdministrativeAreaName":"Moscow","Locality":{"LocalityName":"Moscow","DependentLocality":{"DependentLocalityName":"Moscow Kremlin","Thoroughfare":{"ThoroughfareName":"Troitskaya Street"}}}}}}}},"description":"Moscow Kremlin, Moscow, Russian Federation","name":"Troitskaya Street","boundedBy":{"Envelope":{"lowerCorner":"37.615093 55.751938","upperCorner":"37.617842 55.752273"}},"Point":{"pos":"37.616476 55.752202"}}}]}}}
do_parse_body(State, #{<<"response">> := #{<<"GeoObjectCollection">> := #{<<"metaDataProperty">> := #{<<"GeocoderResponseMetaData">> := #{<<"found">> := Found}}, <<"featureMember">> := FeatureMember}}}) ->
    case erlang:binary_to_integer(Found) of
        0 ->
            {reply, {error, "Unknown address"}, State};
        _ ->
            [#{<<"GeoObject">> := #{<<"description">> := Description}}] = FeatureMember,
            {reply, {ok, Description}, State}
    end.

do_create_url(UrlTemplate, Params) ->
    lists:foldr(fun({Key, Value}, Url) ->
                        binary:replace(Url, Key, Value)
                end, UrlTemplate, Params).