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
-module(em_gps103_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("em_hardware.hrl").
-include("em_records.hrl").

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


-ifdef(TEST).
-compile(export_all).
-endif. % EXPORT_ALL

-define(TIMEOUT, infinity).

-define(SERVER, ?MODULE).

%% PATTERN: imei:(\d+),([^,]+),(\d{2})/?(\d{2})/?(\d{2}) ?(\d{2}):?(\d{2})(?:\d{2})?,([^,]+)?,[FL],(?:(\d{2})(\d{2})(\d{2})\.(\d+)|(?:\d{1,5}\.\d+)?),([AV]),(?:([NS]),)?(\d+)(\d{2}\.\d+),(?:([NS]),)?(?:([EW]),)?(\d+)(\d{2}\.\d+),(?:([EW])?,)?(\d+\.?\d*)?,?(\d+\.?\d*)?,?(\d+\.?\d*)?,?([^,;]+)?,?([^,;]+)?,?([^,;]+)?,?([^,;]+)?,?([^,;]+)?,?.*
%% PATTERN_NETWORK: imei:(\d+),[^,]+,\d*,,L,,,([0-9a-fA-F]+),,([0-9a-fA-F]+),,,.*
%% PATTERN_HANDSHAKE: ##,imei:(\d+),A
%% PATTERN_OBD: imei:(\d+),OBD,(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2}),(\d+),(\d+\.\d+)?,(?:\d+\.\d+)?,(\d+),\d+,(\d+\.\d+%),(\d+),(\d+\.\d+%),[^,]*,[^,]*,[^,]*,[^,]*.*
-define(PATTERN_TEST, list_to_binary([
    "imei:" ++
    "(\\d+)," ++                         %% imei
    "([^,]+)," ++                       %% alarm
    "(\\d{2})/?(\\d{2})/?(\\d{2}) ?" ++    %% locale date
    "(\\d{2}):?(\\d{2}):?(\\d{2})?," ++    %% locale time
    ".*"
])).

-define(PATTERN, list_to_binary([
    "imei:" ++
    "(\\d+)," ++                         %% imei
    "([^,]+)," ++                       %% alarm
    "(\\d{2})/?(\\d{2})/?(\\d{2}) ?" ++    %% locale date
    "(\\d{2}):?(\\d{2}):?(\\d{2})?," ++    %% locale time
    "([^,]+)?," ++                      %% rfid
    "[FL]," ++                          %% full/low
    "(?:" ++                            %% group begin
    "(\\d{2})(\\d{2})(\\d{2})\\.(\\d+)" ++   %% time utc (hhmmss.sss)
    "|" ++                              %% or
    "(?:\\d{1,5}\\.\\d+)?" ++
    ")" ++                              %% group end
    "," ++
    "([AV])," ++                        %% validity
    "(?:([NS]),)?" ++
    "(\\d+)(\\d{2}\\.\\d+)," ++             %% latitude (ddmm.mmmm)
    "(?:([NS]),)?" ++
    "(?:([EW]),)?" ++
    "(\\d+)(\\d{2}\\.\\d+)," ++             %% longitude (dddmm.mmmm)
    "(?:([EW])?,)?" ++
    "(\\d+\\.?\\d*)?,?" ++                 %% speed
    "(\\d+\\.?\\d*)?,?" ++                 %% course
    "(\\d+\\.?\\d*)?,?" ++                  %% altitude
    "([^,;]+)?,?" ++
    "([^,;]+)?,?" ++
    "([^,;]+)?,?" ++
    "([^,;]+)?,?" ++
    "([^,;]+)?,?" ++
    ".*"
])).

-define(PATTERN_NETWORK, list_to_binary([
    "imei:" ++
    "(\\d+)," ++            %% imei
    "[^,]+," ++             %% alarm
    "\\d*,," ++
    "L,,," ++
    "([0-9a-fA-F]+),," ++   %% lac
    "([0-9a-fA-F]+),,," ++  %% cid
    ".*"
])).

-define(PATTERN_HANDSHAKE, list_to_binary([
  "##,imei:(\\d+),A"
])).

-define(PATTERN_OBD, list_to_binary([
    "imei:" ++
    "(\\d+)," ++                    %% imei
    "OBD," ++                       %% type
    "(\\d{2})(\\d{2})(\\d{2})" ++   %% date
    "(\\d{2})(\\d{2})(\\d{2})," ++  %% time
    "(\\d+)," ++                    %% odometer
    "(\\d+\\.\\d+)?," ++            %% fuel instant
    "(?:\\d+\\.\\d+)?," ++          %% fuel average
    "(\\d+)," ++                    %% speed
    "\\d+," ++                      %% power load
    "(\\d+\\.\\d+%)," ++            %% throttle
    "(\\d+)," ++                    %% rpm
    "(\\d+\\.\\d+%)," ++            %% battery
    "[^,]*," ++                     %% dtc 1
    "[^,]*," ++                     %% dtc 2
    "[^,]*," ++                     %% dtc 3
    "[^,]*" ++                      %% dtc 4
    ".*"
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

do_process_data(State = #state{transport = Transport, socket = Socket, protocol = _Protocol, deviceId = 0}, Data = <<"##", _/binary>>) ->
  em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_hardware:resolve(Socket), Data]),
  case parse(Data) of
    {ok, Imei} ->
      em_logger:info("[packet] unit: ip = '~s' imei = '~s'", [em_hardware:resolve(Socket), Imei]),
      case em_data_manager:get_device_by_uid(Imei) of
        {error, _Reason} ->
          em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_hardware:resolve(Socket), Imei]),
          {stop, normal, State};
        {ok, Object} ->
          em_proc:registry(Object#device.id, self()),
          Transport:send(Socket, <<"LOAD">>),
          {noreply, State#state{deviceId = Object#device.id}, ?TIMEOUT}
      end;
    {error, Message} ->
      em_logger:info("ERROR: ~s", [Message]),
      {stop, normal, State}
  end;
do_process_data(State = #state{transport = Transport, socket = Socket, protocol = _Protocol}, Data = <<"##", _/binary>>) ->
  em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_hardware:resolve(Socket), Data]),
  case parse(Data) of
    {ok, Imei} ->
      em_logger:info("[packet] unit: ip = '~s' imei = '~s'", [em_hardware:resolve(Socket), Imei]),
      Transport:send(Socket, <<"LOAD">>),
      {noreply, State, ?TIMEOUT};
    {error, Message} ->
      em_logger:info("ERROR: ~s", [Message]),
      {stop, normal, State}
  end;
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


%% echo "1. gps103"
%% (echo -n -e "##,imei:359586015829802,A";) | nc -v localhost 5001
%% (echo -n -e "imei:123456789012345,help me,1201011201,,F,120100.000,A,6000.0000,N,13000.0000,E,0.00,;";) | nc -v localhost 5001
parse(Data = <<"##", _/binary>>) ->
  case em_regexp:match(Data, ?PATTERN_HANDSHAKE) of
    {ok, [_, Imei | _]} ->
      {ok, Imei};
    Reason ->
      Reason
  end;
parse(Data) ->
  case em_regexp:match(Data, ?PATTERN) of
    {ok, [_, Imei, Alarm, LocalYear, LocalMonth, LocalDay,LocalHour,LocalMinute, LocalSecond, _Rfid ,UtcHour, UtcMinute, UtcSecond, UtcMillseconds,Validity,LatHem0,LatDD,LatMM_MMMM,LatHem1,LonHem0,LonDDD,LonMM_MMMM,LonHem1,Speed|_]} ->
      Position = #position{
        deviceTime = parse_date_time(LocalYear, LocalMonth, LocalDay,LocalHour,LocalMinute, LocalSecond, UtcHour, UtcMinute, UtcSecond, UtcMillseconds),
        latitude = parse_coordinate(hen_deg_min_hem, {LatHem0, LatDD, LatMM_MMMM, LatHem1}),
        longitude = parse_coordinate(hen_deg_min_hem, {LonHem0, LonDDD, LonMM_MMMM, LonHem1}),
        speed = parse_speed(Speed),
        course = 0,
        valid = parse_valid(Validity),
        attributes = #{
          ?KEY_ALARM => Alarm
        }
      },
      {ok, Imei, Position};
    Reason ->
      Reason
  end.


parse_date_time(LocalYear, LocalMonth, LocalDay,LocalHour,LocalMinute, _LocalSecond, UtcHour, UtcMinute, UtcSecond, _UtcMillseconds) ->
  DeltaMinutes = calc_delta_minutes(
    list_to_integer(binary_to_list(LocalHour)),
    list_to_integer(binary_to_list(LocalMinute)),
    list_to_integer(binary_to_list(UtcHour)),
    list_to_integer(binary_to_list(UtcMinute))
  ),
  Date = {
    {
      list_to_integer(binary_to_list(LocalYear)) + 2000,
      list_to_integer(binary_to_list(LocalMonth)),
      list_to_integer(binary_to_list(LocalDay))
    },
    {
      list_to_integer(binary_to_list(LocalHour)),
      list_to_integer(binary_to_list(LocalMinute)) - DeltaMinutes,
      list_to_integer(binary_to_list(UtcSecond))
    }
  },
  em_helper_time:datetime_to_utc(Date).

parse_coordinate(hen_deg_min_hem, {void, Deg, Min, Hem}) ->
  hemisphere(list_to_integer(binary_to_list(Deg)) + list_to_float(binary_to_list(Min)) / 60, Hem);
parse_coordinate(hen_deg_min_hem, {Hem, Deg, Min, void}) ->
  hemisphere(list_to_integer(binary_to_list(Deg)) + list_to_float(binary_to_list(Min)) / 60, Hem).

hemisphere(Coordinate, <<"S">>) -> -1 * Coordinate;
hemisphere(Coordinate, <<"W">>) -> -1 * Coordinate;
hemisphere(Coordinate, <<"-">>) -> -1 * Coordinate;
hemisphere(Coordinate, _) -> Coordinate.


calc_delta_minutes(LocalHours, LocalMinutes, UtcHours, UtcMinutes) ->
  delta((LocalHours - UtcHours) * 60 + LocalMinutes - UtcMinutes).

delta(DeltaMinutes) when (DeltaMinutes =< -12 * 60) ->
  DeltaMinutes + 24 * 60;
delta(DeltaMinutes) when (DeltaMinutes > 12 * 60) ->
  DeltaMinutes - 24 * 60;
delta(DeltaMinutes) ->
  DeltaMinutes.

parse_speed(Speed) ->
  list_to_float(binary_to_list(Speed)).

parse_valid(Validity) when is_binary(Validity) ->
  binary_to_atom(Validity, utf8).


%% ##,imei:359586015829802,A
%% ##,imei:123456789012345,A
%% imei:123456789012345,help me,1201011201,,F,120100.000,A,6000.0000,N,13000.0000,E,0.00,;
%% imei:359710049092324,tracker,151027025958,,F,235957.000,A,2429.5156,N,04424.5828,E,0.01,27.91,,0,0,,,;
%%test() ->
%%  Packet = <<"imei:123456789012345,help me,1201011201,,F,120100.000,A,6000.0000,N,13000.0000,E,0.00,;">>,
%%  %%Packet = <<"##,imei:359586015829802,A">>,
%%  em_regexp:match(Packet, ?PATTERN).