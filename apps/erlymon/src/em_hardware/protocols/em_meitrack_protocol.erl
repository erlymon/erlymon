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
-module(em_meitrack_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("em_hardware.hrl").
-include("em_records.hrl").

%% API
-export([start_link/4]).
-export([test/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(TIMEOUT, infinity).

-define(SERVER, ?MODULE).


-define(PATTERN, list_to_binary([
    "\\$\\$." ++                    %% flag
    "\\d+," ++                      %% length
    "(\\d+)," ++                    %% imei
    "[0-9a-fA-F]{3}," ++            %% command
    "(?:\\d+,)?(\\d+)," ++          %% event
    "(-?\\d+\\.\\d+)," ++           %% latitude
    "(-?\\d+\\.\\d+)," ++           %% longitude
    "(\\d{2})(\\d{2})(\\d{2})" ++   %% date (ddmmyy)
    "(\\d{2})(\\d{2})(\\d{2})," ++  %% time
    "([AV])," ++                    %% validity
    "(\\d+)," ++                    %% satellites
    "(\\d+)," ++                    %% gsm signal
    "(\\d+\\.?\\d*)," ++            %% speed
    "(\\d+)," ++                    %% course
    "(\\d+\\.?\\d*)," ++            %% hdop
    "(-?\\d+)," ++                  %% altitude
    "(\\d+)," ++                    %% odometer
    "(\\d+)," ++                    %% runtime
    "(\\d+)\\|" ++                  %% mc
    "(\\d+)\\|" ++                  %% mnc
    "([0-9a-fA-F]+)\\|" ++          %% lac
    "([0-9a-fA-F]+)," ++            %% cell
    "([0-9a-fA-F]+)," ++            %% state
    "([0-9a-fA-F]+)?\\|" ++         %% adc1
    "([0-9a-fA-F]+)?\\|" ++         %% adc2
    "([0-9a-fA-F]+)?\\|" ++         %% adc3
    "([0-9a-fA-F]+)\\|" ++          %% battery
    "([0-9a-fA-F]+)," ++            %% power
    ".*?"                           %% any
])).

-record(state, {protocol, transport, socket, timeout, deviceId = 0}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Ref :: any(), Socket :: any(), Transport :: any(), Opts :: any()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Ref, Socket, Transport, Opts) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

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
init({Ref, Socket, Transport, Opts}) ->
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, once}]),
  Protocol = proplists:get_value(protocol, Opts),
  gen_server:enter_loop(?MODULE, [],
    #state{protocol = Protocol, socket=Socket, transport=Transport}, ?TIMEOUT).
%%{ok, #state{}}.

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
handle_info({command, Data}, State=#state{socket=Socket, transport=Transport}) ->
  Transport:send(Socket, Data),
  {noreply, State, ?TIMEOUT};
handle_info({tcp, _, ?TRASH}, State=#state{socket=Socket, transport=Transport}) ->
  Transport:setopts(Socket, [{active, once}]),
  {noreply, State, ?TIMEOUT};
handle_info({tcp, Socket, Data}, State=#state{protocol = Protocol, socket=Socket, transport=Transport, deviceId = 0}) when byte_size(Data) > 1 ->
  Transport:setopts(Socket, [{active, once}]),

  em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_hardware:resolve(Socket), Data]),
  case parse(Data) of
    {ok, Imei, PositionModel} ->
      em_logger:info("[packet] unit: ip = '~s' imei = '~s' message: ~w", [em_hardware:resolve(Socket), Imei, PositionModel]),
      case em_data_manager:get_device_by_uid(Imei) of
        {error, _Reason} ->
          em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_hardware:resolve(Socket), Imei]),
          {stop, normal, State};
        {ok, Object} ->
          em_proc:registry(Object#device.id, self()),
          Position = PositionModel#position{
            deviceId = Object#device.id,
            protocol = atom_to_binary(Protocol, utf8),
            attributes = maps:merge(PositionModel#position.attributes, #{
              ?KEY_IP => em_hardware:resolve(Socket)
            })
          },
          em_logger:info("save message => unit: ip = '~s' id = '~w' imei = '~s' position: ~w", [em_hardware:resolve(Socket), Object#device.id, Imei, Position]),
          em_data_manager:create_position(Object#device.id, Position),
          {noreply, State#state{deviceId = Object#device.id}, ?TIMEOUT}
      end;
    {error, Message} ->
      em_logger:info("ERROR: ~s", [Message]),
      {stop, normal, State}
  end;
handle_info({tcp, Socket, Data}, State=#state{protocol = Protocol, socket=Socket, transport=Transport, deviceId = DeviceId}) when byte_size(Data) > 1 ->
  Transport:setopts(Socket, [{active, once}]),

  em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_hardware:resolve(Socket), Data]),
  case parse(Data) of
    {ok, Imei, PositionModel} ->
      em_logger:info("[packet] unit: ip = '~s' imei = '~s' message: ~w", [em_hardware:resolve(Socket), Imei, PositionModel]),
      Position = PositionModel#position{
        deviceId = DeviceId,
        protocol = atom_to_binary(Protocol, utf8),
        attributes = maps:merge(PositionModel#position.attributes, #{
          ?KEY_IP => em_hardware:resolve(Socket)
        })
      },
      em_logger:info("save message => unit: ip = '~s' id = '~w' imei = '~s' position: ~w", [em_hardware:resolve(Socket), DeviceId, Imei, Position]),
      em_data_manager:create_position(DeviceId, Position),
      {noreply, State, ?TIMEOUT};
    {error, Message} ->
      em_logger:info("ERROR: ~s", [Message]),
      {stop, normal, State}
  end;
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
  {stop, Reason, State};
handle_info(timeout, State) ->
  {stop, normal, State};
handle_info(_Info, State) ->
  {stop, normal, State}.
%%handle_info(_Info, State) ->
%%  {noreply, State}.

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

%% echo "20. meitrack"
%% (echo -n -e "\$\$d138,123456789012345,AAA,35,60.000000,130.000000,120101122000,A,7,18,0,0,0,49,3800,24965,510|10|0081|4F4F,0000,000D|0010|0012|0963|0000,,*BF\r\n";) | nc -v localhost 5020
parse(Data) ->
  case em_regexp:match(Data, ?PATTERN) of
    {ok, [_, Imei, _Event, Latitude, Longitude, Year, Month, Day, Hour, Minute, Second, Validity, Satellites, _GsmSignal, Speed, Course, _Hdop, Altitude, _Odometer, _Runtime, _Cell, _State, _Adc1, _Adc2, _Adc3, _Battery, Power | _]} ->
      Position = #position{
        deviceTime = parse_date(Year, Month, Day, Hour, Minute, Second),
        latitude = list_to_float(binary_to_list(Latitude)),
        longitude = list_to_float(binary_to_list(Longitude)),
        altitude = parse_altitude(Altitude),
        speed = parse_speed(Speed),
        course = parse_course(Course),
        valid = parse_valid(Validity),
        attributes = #{
          ?KEY_SATELLITES => parse_satellites(Satellites),
          ?KEY_POWER => parse_power(Power)
        }
      },
      {ok, Imei, Position};
    Reason ->
      Reason
  end.

parse_satellites(Satteliates) ->
  list_to_integer(binary_to_list(Satteliates)).

parse_power(Power) ->
  em_logger:info("PARSER POWER: '~w'", [Power]),
  erlang:binary_to_integer(Power, 16).
%%list_to_integer(binary_to_list(Power)).

parse_altitude(Altitude) ->
  list_to_integer(binary_to_list(Altitude)).

parse_speed(Speed) ->
  list_to_integer(binary_to_list(Speed)).

parse_course(Course) ->
  list_to_integer(binary_to_list(Course)).

parse_valid(Validity) when Validity == <<"A">> ->
  true;
parse_valid(Validity) when Validity == <<"V">> ->
  false.


%parse_device_id(DeviceId) ->
%    list_to_integer(binary_to_list(DeviceId)).

parse_date(Year, Month, Day, Hour, Minute, Second) ->
  Date = {
    {
      list_to_integer(binary_to_list(Year)) + 2000,
      list_to_integer(binary_to_list(Month)),
      list_to_integer(binary_to_list(Day))
    },
    {
      list_to_integer(binary_to_list(Hour)),
      list_to_integer(binary_to_list(Minute)),
      list_to_integer(binary_to_list(Second))
    }
  },
  em_helper_time:datetime_to_utc(Date).

%% $$J139,359231038158125,AAA,35,53.897721,27.443013,161125211605,A,5,30,0,4,3.4,252,1070708,1506398,257|4|0000|0000,0000,0007|0007||02DD|00FE,*E8
%% $$K139,359231038158125,AAA,35,53.897721,27.443013,161125211605,A,5,30,0,4,3.4,252,1070708,1506398,257|4|0000|0000,0000,0007|0007||02DD|00FE,*E9
test() ->
  %%Packet = <<"\$\$d138,123456789012345,AAA,35,60.000000,130.000000,120101122000,A,7,18,0,0,0,49,3800,24965,510|10|0081|4F4F,0000,000D|0010|0012|0963|0000,,*BF\r\n">>,
  Packet = <<"\$\$K139,359231038158125,AAA,35,53.897721,27.443013,161126081580,A,5,30,0,4,3.4,252,1070708,1506398,257|4|0000|0000,0000,0007|0007||02DD|00FE,*E9\r\n">>,
  em_regexp:match(Packet, ?PATTERN).
