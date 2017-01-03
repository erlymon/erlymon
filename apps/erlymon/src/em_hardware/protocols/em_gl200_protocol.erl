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
-module(em_gl200_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("em_hardware.hrl").
-include("em_records.hrl").

%% API
-export([start_link/4]).
%%-export([test/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(TIMEOUT, infinity).

-define(SERVER, ?MODULE).

%% PATTERN_HBD '\+ACK:GTHBD,([0-9A-Z]{2}[0-9a-fA-F]{4}),.*,([0-9a-fA-F]{4})(?:\$)?'
%% PATTERN_INF '\+RESP:GTINF,[0-9A-Z]{2}[0-9a-fA-F]{4},(\d{15}),[0-9A-Z]{17},[^,]{0,20},([0-9a-fA-F]{2}),[0-9F]{20},\d{1,2},\d{1,2},[01],(\d{1,5}),,(\d+\.\d+),([01]),[01],,,\d{14},,,,,,[-+]\d{4},[01],(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2}),([0-9a-fA-F]{4})(?:\$)?'
%% PATTERN_LOCATION '(?:\d{1,2})?,(\d{1,3}\.\d)?,(\d{1,3})?,(-?\d{1,5}\.\d)?,(-?\d{1,3}\.\d{6})?,(-?\d{1,2}\.\d{6})?,(?:(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2}))?,(?:(0\d{3})?,(0\d{3})?,([0-9a-fA-F]{4})?,([0-9a-fA-F]{4})?,|(\d+)?,(\d+)?,(\d+)?,(\d+)?,)(?:\d+|(\d+\.\d))?,'
%% PATTERN_OBD '\+RESP:GTOBD,[0-9A-Z]{2}[0-9a-fA-F]{4},(\d{15}),(?:[0-9A-Z]{17})?,[^,]{0,20},[01],[0-9a-fA-F]{1,8},(?:[0-9A-Z]{17})?,[01],(?:\d{1,5})?,(?:[0-9a-fA-F]{8})?,(\d{1,5})?,(\d{1,3})?,(-?\d{1,3})?,(\d+\.?\d*|Inf|NaN)?,(\d{1,5})?,(?:\d{1,5})?,([01])?,(\d{1,3})?,([0-9a-fA-F]*),(\d{1,3})?,(?:\d{1,3})?,(\d{1,3})?,(\d+),(?:\d{1,2})?,(\d{1,3}\.\d)?,(\d{1,3})?,(-?\d{1,5}\.\d)?,(-?\d{1,3}\.\d{6})?,(-?\d{1,2}\.\d{6})?,(?:(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2}))?,(?:(0\d{3})?,(0\d{3})?,([0-9a-fA-F]{4})?,([0-9a-fA-F]{4})?,|(\d+)?,(\d+)?,(\d+)?,(\d+)?,)(?:\d+|(\d+\.\d))?,(\d{1,7}\.\d)?,(?:(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2}))?,([0-9a-fA-F]{4})(?:\$)?'
%% PATTERN_FRI '\+(?:RESP|BUFF):GTFRI,(?:[0-9A-Z]{2}[0-9a-fA-F]{4})?,(\d{15}|[0-9a-fA-F]{14}),[^,]*,(\d+)?,\d{1,2},\d{1,2},((?:(?:\d{1,2})?,(\d{1,3}\.\d)?,(\d{1,3})?,(-?\d{1,5}\.\d)?,(-?\d{1,3}\.\d{6})?,(-?\d{1,2}\.\d{6})?,(?:(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2}))?,(?:(0\d{3})?,(0\d{3})?,([0-9a-fA-F]{4})?,([0-9a-fA-F]{4})?,|(\d+)?,(\d+)?,(\d+)?,(\d+)?,)(?:\d+|(\d+\.\d))?,)+)(?:(?:(\d{1,7}\.\d)?,)?(\d{1,3})?,|(\d{1,7}\.\d)?,(\d{5}:\d{2}:\d{2}),([0-9a-fA-F]+)?,([0-9a-fA-F]+)?,(\d{1,3})?,(\d{6})?,,,,)(?:(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2}))?,([0-9a-fA-F]{4})(?:\$)?'
%% PATTERN '\+(?:RESP|BUFF):GT...,(?:[0-9A-Z]{2}[0-9a-fA-F]{4})?,(\d{15}),[^,]*,\d*,\d{1,2},\d{1,2},(?:\d{1,2})?,(\d{1,3}\.\d)?,(\d{1,3})?,(-?\d{1,5}\.\d)?,(-?\d{1,3}\.\d{6})?,(-?\d{1,2}\.\d{6})?,(?:(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2}))?,(?:(0\d{3})?,(0\d{3})?,([0-9a-fA-F]{4})?,([0-9a-fA-F]{4})?,|(\d+)?,(\d+)?,(\d+)?,(\d+)?,)(?:\d+|(\d+\.\d))?,(?:(?:(\d{1,7}\.\d)?,)?(\d{1,3})?,|(\d{1,7}\.\d)?,)(?:(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2}))?,([0-9a-fA-F]{4})(?:\$)?'
%% PATTERN_BASIC '\+(?:RESP|BUFF):GT...,(?:[0-9A-Z]{2}[0-9a-fA-F]{4})?,(\d{15}|[0-9a-fA-F]{14}),.*(\d{1,3}\.\d)?,(\d{1,3})?,(-?\d{1,5}\.\d)?,(-?\d{1,3}\.\d{6}),(-?\d{1,2}\.\d{6}),(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2}),.*([0-9a-fA-F]{4})(?:\$)?'

-define(PATTERN_HBD, list_to_binary([
    "\\+ACK:GTHBD," ++
    "([0-9A-Z]{2}[0-9a-fA-F]{4})," ++
    ".*" ++
    "," ++
    "([0-9a-fA-F]{4})" ++
    "(?:\\$)?"
])).

-define(PATTERN_INF, list_to_binary([
    "\\+RESP:GTINF," ++
    "[0-9A-Z]{2}[0-9a-fA-F]{4}," ++   %% protocol version
    "(\\d{15})," ++                    %% imei
    "[0-9A-Z]{17}," ++                %% vin
    "[^,]{0,20}," ++                  %% device name
    "([0-9a-fA-F]{2})," ++            %% state
    "[0-9F]{20}," ++                  %% iccid
    "\\d{1,2}," ++
    "\\d{1,2}," ++
    "[01]," ++
    "(\\d{1,5})," ++                   %% power
    "," ++
    "(\\d+\\.\\d+)," ++                  %% battery
    "([01])," ++                      %% charging
    "[01]," ++
    ",," ++
    "\\d{14}," ++                      %% last fix time
    ",,,,," ++
    "[-+]\\d{4}," ++                   %% timezone
    "[01]," ++                        %% dayling saving
    "(\\d{4})(\\d{2})(\\d{2})" ++        %% date
    "(\\d{2})(\\d{2})(\\d{2})," ++       %% time
    "([0-9a-fA-F]{4})" ++             %% counter
    "(?:\\$)?"
])).

-define(PATTERN_LOCATION, list_to_binary([
    "(?:\\d{1,2})?," ++            %% gps accuracy
    "(\\d{1,3}\\.\\d)?," ++        %% speed
    "(\\d{1,3})?," ++              %% course
    "(-?\\d{1,5}\\.\\d)?," ++      %% altitude
    "(-?\\d{1,3}\\.\\d{6})?," ++   %% longitude
    "(-?\\d{1,2}\\.\\d{6})?," ++   %% latitude
    "(?:" ++                       %% group begin
    "(\\d{4})(\\d{2})(\\d{2})" ++  %% date
    "(\\d{2})(\\d{2})(\\d{2})" ++  %% time
    ")?" ++                        %% group end
    "," ++
    "(?:" ++                       %% group begin
    "(0\\d{3})?," ++               %% mcc
    "(0\\d{3})?," ++               %% mnc
    "([0-9a-fA-F]{4})?," ++     %% lac
    "([0-9a-fA-F]{4})?," ++     %% cell
    "|" ++                      %% or
    "(\\d+)?," ++                %% mcc
    "(\\d+)?," ++                %% mnc
    "(\\d+)?," ++                %% lac
    "(\\d+)?," ++                %% cell
    ")" ++                      %% group end
    "(?:\\d+|(\\d+\\.\\d))?,"       %% odometer
])).

-define(PATTERN_OBD, list_to_binary([
    "\\+RESP:GTOBD," ++
    "[0-9A-Z]{2}[0-9a-fA-F]{4}," ++   %% protocol version
    "(\\d{15})," ++                    %% imei
    "(?:[0-9A-Z]{17})?," ++           %% vin
    "[^,]{0,20}," ++                  %% device name
    "[01]," ++                        %% report type
    "[0-9a-fA-F]{1,8}," ++            %% report mask
    "(?:[0-9A-Z]{17})?," ++           %% vin
    "[01]," ++                        %% obd connect
    "(?:\\d{1,5})?," ++                %% obd voltage
    "(?:[0-9a-fA-F]{8})?," ++         %% support pids
    "(\\d{1,5})?," ++                  %% engine rpm
    "(\\d{1,3})?," ++                  %% speed
    "(-?\\d{1,3})?," ++                %% coolant temp
    "(\\d+\\.?\\d*|Inf|NaN)?," ++        %% fuel consumption
    "(\\d{1,5})?," ++                  %% dtcs cleared distance
    "(?:\\d{1,5})?," ++
    "([01])?," ++                     %% obd connect
    "(\\d{1,3})?," ++                  %% number of dtcs
    "([0-9a-fA-F]*)," ++              %% dtcs
    "(\d{1,3})?," ++                  %% throttle
    "(?:\\d{1,3})?," ++                %% engine load
    "(\\d{1,3})?," ++                  %% fuel level
    "(\\d+)," ++                       %% odometer
    binary_to_list(?PATTERN_LOCATION) ++
    "(\\d{1,7}\\.\\d)?," ++              %% odometer
    "(?:" ++                          %% group begin
    "(\\d{4})(\\d{2})(\\d{2})" ++        %% date
    "(\\d{2})(\\d{2})(\\d{2})" ++        %% time
    ")?" ++                           %% group end
    "," ++
    "([0-9a-fA-F]{4})" ++             %% count number
    "(?:\\$)?"
])).

-define(PATTERN_FRI, list_to_binary([
    "\\+" ++
    "(?:RESP|BUFF):GTFRI," ++
    "(?:[0-9A-Z]{2}[0-9a-fA-F]{4})?," ++  %% protocol version
    "(\\d{15}|[0-9a-fA-F]{14})," ++        %% imei
    "[^,]*," ++                           %% device name
    "(\\d+)?," ++                          %% power
    "\\d{1,2}," ++                         %% report type
    "\\d{1,2}," ++                         %% count
    "((?:" ++
    binary_to_list(?PATTERN_LOCATION) ++
    ")+)" ++
    "(?:" ++                              %% group begin
    "(?:" ++                              %% group begin
    "(\\d{1,7}\\.\\d)?," ++                  %% odometer
    ")?" ++                               %% group end
    "(\\d{1,3})?," ++                      %% battery
    "|" ++                                %% or
    "(\\d{1,7}\\.\\d)?," ++                  %% odometer
    "(\\d{5}:\\d{2}:\\d{2})," ++             %% hour meter
    "([0-9a-fA-F]+)?," ++                 %% adc 1
    "([0-9a-fA-F]+)?," ++                 %% adc 2
    "(\\d{1,3})?," ++                      %% battery
    "(\\d{6})?,,,," ++                     %% device status
    ")" ++                                %% group end
    "(?:" ++
    "(\\d{4})(\\d{2})(\\d{2})" ++            %% date
    "(\\d{2})(\\d{2})(\\d{2})" ++            %% time
    ")?" ++
    "," ++
    "([0-9a-fA-F]{4})" ++                 %% count number
    "(?:\\$)?"
])).

-define(PATTERN, list_to_binary([
    "\\+(?:RESP|BUFF):GT...," ++
    "(?:[0-9A-Z]{2}[0-9a-fA-F]{4})?," ++  %% protocol version
    "(\\d{15})," ++                        %% imei
    "[^,]*," ++                           %% device name
    "\\d*," ++
    "\\d{1,2}," ++                         %% report type
    "\\d{1,2}," ++                         %% count
    binary_to_list(?PATTERN_LOCATION) ++
    "(?:" ++                              %% group begin
    "(?:" ++                              %% group begin
    "(\\d{1,7}\\.\\d)?," ++                  %% odometer
    ")?" ++                               %% group end
    "(\\d{1,3})?," ++                      %% battery
    "|" ++                                %% or
    "(\\d{1,7}\\.\\d)?," ++                  %% odometer
    ")" ++                                %% group end
    "(?:" ++                              %% group begin
    "(\\d{4})(\\d{2})(\\d{2})" ++            %% date
    "(\\d{2})(\\d{2})(\\d{2})" ++            %% time
    ")?" ++                               %% group end
    "," ++
    "([0-9a-fA-F]{4})" ++                 %% count number
    "(?:\\$)?"
])).

-define(PATTERN_BASIC, list_to_binary([
    "\\+" ++
    "(?:RESP|BUFF)" ++
    ":" ++
    "GT...," ++
    "(?:[0-9A-Z]{2}[0-9a-fA-F]{4})?," ++    %% protocol version
    "(\\d{15}|[0-9a-fA-F]{14})," ++          %% imei
    ".*" ++                                 %% any
    "(\\d{1,3}\\.\\d)?," ++                    %% speed
    "(\\d{1,3})?," ++                        %% course
    "(-?\\d{1,5}\\.\\d)?," ++                  %% altitude
    "(-?\\d{1,3}\\.\\d{6})," ++                %% longitude
    "(-?\\d{1,2}\\.\\d{6})," ++                %% latitude
    "(\\d{4})(\\d{2})(\\d{2})" ++              %% date
    "(\\d{2})(\\d{2})(\\d{2})" ++              %% time
    "," ++
    ".*" ++
    "([0-9a-fA-F]{4})" ++                   %% count number
    "(?:\\$)?"
])).

-define(SOCKET_OPTS, [{active, once}, {packet, line}]).

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
  ok = Transport:setopts(Socket, ?SOCKET_OPTS),
  Protocol = proplists:get_value(protocol, Opts),
  gen_server:enter_loop(?MODULE, [],
    #state{protocol = Protocol, socket = Socket, transport = Transport}, ?TIMEOUT).
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
handle_info({command, Command}, State) ->
  do_execute_command(State, Command);
handle_info({tcp, _, <<67,78,88,78,0,0,0,1,0,0,4,0,27,0,0,0,77,10>>}, State = #state{socket = Socket, transport = Transport}) ->
  Transport:setopts(Socket, ?SOCKET_OPTS),
  {noreply, State, ?TIMEOUT};
handle_info({tcp, Socket, Data}, State = #state{socket = Socket, transport = Transport}) when byte_size(Data) > 1 ->
  Transport:setopts(Socket, ?SOCKET_OPTS),
  do_process_data(State, Data);
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
do_execute_command(State = #state{transport = Transport, socket = Socket}, Command) ->
  case em_manager_devices:get_by_id(Command#command.deviceId) of
    {ok, Device} ->
      case do_encode_command(Device#device.uniqueId, Command) of
        {ok, CommandBin} ->
          em_logger:info("CommandBin: ~s", [CommandBin]),
          Transport:send(Socket, CommandBin),
          {noreply, State, ?TIMEOUT};
        {error, Reason} ->
          em_logger:info("Error: ~s", [Reason]),
          {noreply, State, ?TIMEOUT}
      end;
    {error, Reason} ->
      em_logger:info("Error: ~s", [Reason]),
      {noreply, State, ?TIMEOUT}
  end.

do_encode_command(_UniqueId, _Command) ->
  {error, <<"Unsupported command">>}.

do_process_data(State = #state{transport = _Transport, socket = Socket, protocol = _Protocol, deviceId = 0}, Data) ->
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
          do_save_position(State#state{deviceId = Object#device.id}, Object#device.id, Imei, PositionModel)
      end;
    {error, Message} ->
      em_logger:info("ERROR: ~s", [Message]),
      {stop, normal, State}
  end;
do_process_data(State = #state{transport = _Transport, socket = Socket, protocol = _Protocol, deviceId = DeviceId}, Data) ->
  em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_hardware:resolve(Socket), Data]),
  case parse(Data) of
    {ok, Imei, PositionModel} ->
      do_save_position(State, DeviceId, Imei, PositionModel);
    {error, Message} ->
      em_logger:info("ERROR: ~s", [Message]),
      {stop, normal, State}
  end;
do_process_data(State, _) ->
  %%em_logger:info("ERROR: parsing packet"),
  {stop, normal, State}.

do_save_position(State = #state{socket = Socket, protocol = Protocol}, DeviceId, Imei, PositionModel) ->
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
  {noreply, State, ?TIMEOUT}.


% echo "4. gl200"
% (echo -n -e "+RESP:GTFRI,020102,123456789012345,,0,0,1,1,0.0,0,0.0,130.000000,60.000000,20120101120400,0460,0000,18d8,6141,00,,20120101120400,11F0\$";) | nc -v localhost 5004

parse(Data = <<"+RESP:GTFRI", _/binary>>) ->
  case em_regexp:match(Data, ?PATTERN_FRI) of
    {ok, [_, Imei, Validity, Speed, Course, Altitude, Longitude, Latitude, Year, Month, Day, Hour, Minute, Second,  Mcc, Mnc, Lac, Cell | _]} ->
      Position = #position{
        deviceTime = parse_date(Year, Month, Day, Hour, Minute, Second),
        latitude = parse_coord(Latitude),
        longitude = parse_coord(Longitude),
        speed = parse_speed(Speed),
        course = parse_course(Course),
        altitude = parse_altitude(Altitude),
        valid = parse_valid(Validity),
        attributes = #{
          ?KEY_MCC => parse_mcc(Mcc),
          ?KEY_MNC => parse_mnc(Mnc),
          ?KEY_LAC => parse_lac(Lac),
          ?KEY_CELL => parse_cell(Cell)
          %%<<"odometer">> => parse_odometer(Odometer),
          %%<<"battery">> => parse_battery(Battery)
        }
      },
      {ok, Imei, Position};
    Reason ->
      Reason
  end;
parse(_) ->
  {error, <<"Unknown packet">>}.


parse_mcc(Mcc) ->
  Mcc.

parse_mnc(Mnc) ->
  Mnc.

parse_lac(Lac) ->
  Lac.

parse_cell(Cell) ->
  Cell.

%%parse_odometer(Odometer) ->
%%    Odometer.

%%parse_battery(Battery) ->
%%    Battery.

parse_altitude(Altitude) ->
  list_to_float(binary_to_list(Altitude)).

parse_speed(Speed) ->
  list_to_float(binary_to_list(Speed)).

parse_course(Course) ->
  list_to_integer(binary_to_list(Course)).

parse_coord(Coord) ->
  list_to_float(binary_to_list(Coord)).

parse_valid(Validity) when Validity == <<"1">> ->
  true;
parse_valid(Validity) when Validity == <<"0">> ->
  false.



%parse_device_id(DeviceId) ->
%    list_to_integer(binary_to_list(DeviceId)).

parse_date(Year, Month, Day, Hour, Minute, Second) ->
  Date = {
    {
      list_to_integer(binary_to_list(Year)),
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

%% +RESP:GTFRI,020102,123456789012345,,0,0,1,1,0.0,0,0.0,130.000000,60.000000,20120101120400,0460,0000,18d8,6141,00,,20120101120400,11F0\$
%% +RESP:GTFRI,2C0204,867162020003125,GL300W,0,0,2,1,1.7,205,2867.0,-78.481127,-0.206828,20160215210433,0740,0000,7596,5891C,0.0,1,1.7,205,2867.0,-78.481127,-0.206828,20160215210503,0740,0000,7596,5891C,0.0,88,20160215210506,1E78\$
%%test() ->
%%  %%Packet = <<"+RESP:GTFRI,2C0204,867162020003125,GL300W,0,0,2,1,1.7,205,2867.0,-78.481127,-0.206828,20160215210433,0740,0000,7596,5891C,0.0,1,1.7,205,2867.0,-78.481127,-0.206828,20160215210503,0740,0000,7596,5891C,0.0,88,20160215210506,1E78\$">>,
%%  Packet = <<"1,1.7,205,2867.0,-78.481127,-0.206828,20160215210433,0740,0000,7596,5891C,0.0">>,
%%  em_regexp:match(Packet, ?PATTERN_LOCATION).