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

%% API
-export([start_link/2]).
-export([reverse/3]).
-export([test/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).


%% Url: https://maps.googleapis.com/maps/api/geocode/json?latlng=40.714224,-73.961452&key=API_KEY
-define(GOOGLE_COORDS_URL, <<"https://maps.googleapis.com/maps/api/geocode/json?latlng={lat},{lon}&language={lang}">>).
%% Url: https://geocode-maps.yandex.ru/1.x/?geocode=40.714224,-73.961452&format=json&results=1&lang=en_US&key=API_KEY
-define(YANDEX_COORDS_URL, <<"https://geocode-maps.yandex.ru/1.x/?geocode={lon},{lat}&format=json&results=1&lang={lang}">>).
%% Url: http://nominatim.openstreetmap.org/reverse?format=json&lat=40.714224&lon=-73.961452&zoom=18&addressdetails=1
-define(NOMINATIM_COORDS_URL, <<"http://nominatim.openstreetmap.org/reverse?format=json&lat={lat}&lon={lon}&zoom=18&addressdetails=1">>).

-record(state, {type, settings}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(reverse(Latitude :: float(), Longitude :: float(), Language :: string()) -> {ok, string()} | {error, string()}).
reverse(Latitude, Longitude, Language) ->
    gen_server:call(?SERVER, {reverse, Latitude, Longitude, Language}).

test() -> em_geocoder:reverse(55.7522200, 37.6155600, <<"en_US">>).
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
init([GeocoderType, GeocoderSettings]) ->
    em_logger:info("Init '~s' geocoder service", [GeocoderType]),
    {ok, #state{type = GeocoderType, settings = GeocoderSettings}}.

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
terminate(_Reason, _State) ->
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
do_reverse(State = #state{type = google}, Latitude, Longitude, Language) ->
    Headers = [],
    URL = create_url(?GOOGLE_COORDS_URL, [
        {<<"{lat}">>, erlang:float_to_binary(Latitude,[{decimals, 8}])},
        {<<"{lon}">>, erlang:float_to_binary(Longitude,[{decimals, 8}])},
        {<<"{lang}">>, Language}
    ]),
    do_request(State, Headers, URL);
do_reverse(State = #state{type = yandex}, Latitude, Longitude, Language) ->
    Headers = [],
    URL = create_url(?YANDEX_COORDS_URL, [
        {<<"{lat}">>, erlang:float_to_binary(Latitude,[{decimals, 8}])},
        {<<"{lon}">>, erlang:float_to_binary(Longitude,[{decimals, 8}])},
        {<<"{lang}">>, Language}
    ]),
    do_request(State, Headers, URL);
do_reverse(State = #state{type = nominatim}, Latitude, Longitude, Language) ->
    Headers = [{<<"Accept-Language">>, Language}],
    URL = create_url(?NOMINATIM_COORDS_URL, [
        {<<"{lat}">>, erlang:float_to_binary(Latitude,[{decimals, 8}])},
        {<<"{lon}">>, erlang:float_to_binary(Longitude,[{decimals, 8}])}
    ]),
    do_request(State, Headers, URL);
do_reverse(State, _, _, _) ->
    {reply, {error, <<"Unknown provider">>}, State}.


do_request(State, Headers, URL) ->
    %%io:format("URL: ~s", [URL]),
    Method = get,
    Payload = <<>>,
    Options = [{pool, default}],
    case hackney:request(Method, URL, Headers, Payload, Options) of
        {ok, 200, _RespHeaders, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, Body} ->
                    %%io:format("Body:~s", [Body]),
                    do_parse_body(State, jsx:decode(Body, [return_maps]));
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.


do_parse_body(State = #state{type = google}, #{<<"error_message">> := Message}) ->
    {reply, {error, Message}, State};
do_parse_body(State = #state{type = google}, #{<<"results">> := [#{<<"formatted_address">> := FormattedAddress}|_]}) ->
    {reply, {ok, FormattedAddress}, State};
%% {"error":{"status":"400","message":"missing lang parameter"}}
do_parse_body(State = #state{type = yandex}, #{<<"error">> := #{<<"message">> := Message}}) ->
    {reply, {error, Message}, State};
%% {"response":{"GeoObjectCollection":{"metaDataProperty":{"GeocoderResponseMetaData":{"request":"37.6155600,55.7522200","found":"8","results":"1","Point":{"pos":"37.615560 55.752220"}}},"featureMember":[{"GeoObject":{"metaDataProperty":{"GeocoderMetaData":{"kind":"street","text":"Russian Federation, Moscow, Moscow Kremlin, Troitskaya Street","precision":"street","AddressDetails":{"Country":{"AddressLine":"Moscow, Moscow Kremlin, Troitskaya Street","CountryNameCode":"RU","CountryName":"Russian Federation","AdministrativeArea":{"AdministrativeAreaName":"Moscow","Locality":{"LocalityName":"Moscow","DependentLocality":{"DependentLocalityName":"Moscow Kremlin","Thoroughfare":{"ThoroughfareName":"Troitskaya Street"}}}}}}}},"description":"Moscow Kremlin, Moscow, Russian Federation","name":"Troitskaya Street","boundedBy":{"Envelope":{"lowerCorner":"37.615093 55.751938","upperCorner":"37.617842 55.752273"}},"Point":{"pos":"37.616476 55.752202"}}}]}}}
do_parse_body(State = #state{type = yandex}, #{<<"response">> := #{<<"GeoObjectCollection">> := #{<<"metaDataProperty">> := #{<<"GeocoderResponseMetaData">> := #{<<"found">> := Found}}, <<"featureMember">> := FeatureMember}}}) ->
    case erlang:binary_to_integer(Found) of
        0 ->
            {reply, {error, "Unknown address"}, State};
        _ ->
            [#{<<"GeoObject">> := #{<<"description">> := Description}}] = FeatureMember,
            {reply, {ok, Description}, State}
    end;
do_parse_body(State = #state{type = nominatim}, #{<<"display_name">> := DisplayName}) ->
    {reply, {ok, DisplayName}, State};
do_parse_body(State, _) ->
    {reply, {error, "unknown error"}, State}.

create_url(UrlTemplate, Params) ->
    lists:foldr(fun({Key, Value}, Url) ->
        binary:replace(Url, Key, Value)
                end, UrlTemplate, Params).
