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
-module(em_xexun_protocol).
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

-define(TIMEOUT, infinity).

-define(SERVER, ?MODULE).

-define(PATTERN_BASIC, list_to_binary([
    "G[PN]RMC," ++
    "(\\d{2})(\\d{2})(\\d{2})\\.(\\d+)," ++  %% time
    "([AV])," ++                        %% validity
    "(\\d*?)(\\d?\\d\\.\\d+),([NS])," ++     %% latitude
    "(\\d*?)(\\d?\\d\\.\\d+),([EW])?," ++    %% longitude
    "(\\d+\\.?\\d*)," ++                   %% speed
    "(\\d+\\.?\\d*)?," ++                  %% course
    "(\\d{2})(\\d{2})(\\d{2})," ++         %% date
    "[^*]*" ++ "\\*" ++
    "[0-9a-fA-F]{2}" ++                 %% checksum
    "(?:\\r\\n)" ++ "?" ++
    ",([FL])," ++                       %% signal
    "(?:([^,]*),)?" ++                %% alarm
    ".*" ++
    "imei:(\\d+),"                       %% imei
])).

-define(PATTERN_FULL, list_to_binary([
  %%".*" ++
    "(\\d+)," ++                         %% serial
    "([^,]+)?," ++                      %% phone number
    binary_to_list(?PATTERN_BASIC) ++
    "(\\d+)," ++                         %% satellites
    "(-?\\d+\\.\\d+)?," ++                 %% altitude
    "[FL]:(\\d+\\.\\d+)V" ++               %% power
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

do_process_data(State = #state{transport = _Transport, socket = Socket, protocol = _Protocol, deviceId = 0}, Data) ->
  em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_inet:resolve(Socket), Data]),
  case parse(Data) of
    {ok, Imei, PositionModel} ->
      em_logger:info("[packet] unit: ip = '~s' imei = '~s' message: ~w", [em_inet:resolve(Socket), Imei, PositionModel]),
      case em_data_manager:get_device_by_uid(Imei) of
        {error, _Reason} ->
          em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_inet:resolve(Socket), Imei]),
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
  em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_inet:resolve(Socket), Data]),
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
  em_logger:info("[packet] unit: ip = '~s' imei = '~s' message: ~w", [em_inet:resolve(Socket), Imei, PositionModel]),
  Position = PositionModel#position{
    deviceId = DeviceId,
    protocol = atom_to_binary(Protocol, utf8),
    attributes = maps:merge(PositionModel#position.attributes, #{
      ?KEY_IP => em_inet:resolve(Socket)
    })
  },
  em_logger:info("save message => unit: ip = '~s' id = '~w' imei = '~s' position: ~w", [em_inet:resolve(Socket), DeviceId, Imei, Position]),
  em_data_manager:create_position(DeviceId, Position),
  {noreply, State, ?TIMEOUT}.


%% echo "6. xexun"
%% (echo -n -e "111111120009,+436763737552,GPRMC,120600.000,A,6000.0000,N,13000.0000,E,0.00,0.00,010112,,,A*68,F,, imei:123456789012345,04,481.2,F:4.15V,0,139,2689,232,03,2725,0576\n";) | nc -v localhost 5006
parse(Data) ->
  case em_regexp:match(Data, ?PATTERN_FULL) of
    {ok, [_, _Serial, _PhoneNumber, Hour, Minute, Second, _Millisecond, Validity, LatDD, LatMM_MMMM, LatType, LonDD, LonMM_MMMM, LonType, Speed, Course, Day, Month, Year, _Signal, Alarm, Imei, Satellite, Altitude, Power| _]} ->
      Position = #position{
        deviceTime = parse_date(Year, Month, Day, Hour, Minute, Second),
        latitude = parse_coord(LatDD, LatMM_MMMM, LatType),
        longitude = parse_coord(LonDD, LonMM_MMMM, LonType),
        altitude = parse_altitude(Altitude),
        speed = parse_speed(Speed),
        course = parse_course(Course),
        valid = parse_valid(Validity),
        attributes = #{
          ?KEY_POWER => parse_power(Power),
          ?KEY_ALARM => Alarm,
          ?KEY_SATELLITES => parse_sattelite(Satellite)
        }
      },
      {ok, Imei, Position};
    Reason ->
      Reason
  end.



parse_coord(CoordDD, CoordMM_MMMM, CoordType) ->
  Coord = list_to_integer(binary_to_list(CoordDD)) + list_to_float(binary_to_list(CoordMM_MMMM)) / 60,
  case CoordType of
    <<"S">> ->
      Coord * -1;
    <<"N">> ->
      Coord;
    <<"W">> ->
      Coord * -1;
    <<"E">> ->
      Coord
  end.

parse_sattelite(Satteliate) ->
  list_to_integer(binary_to_list(Satteliate)).

parse_power(Power) ->
  list_to_float(binary_to_list(Power)).

parse_altitude(Altitude) ->
  list_to_float(binary_to_list(Altitude)).

parse_speed(Speed) ->
  list_to_float(binary_to_list(Speed)).

parse_course(Course) ->
  list_to_float(binary_to_list(Course)).

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

%% 111111120009,+436763737552,GPRMC,120600.000,A,6000.0000,N,13000.0000,E,0.00,0.00,010112,,,A*68,F,, imei:123456789012345,04,481.2,F:4.15V,0,139,2689,232,03,2725,0576\n
%%test() ->
%%  Packet = <<"111111120009,+436763737552,GPRMC,120600.000,A,6000.0000,N,13000.0000,E,0.00,0.00,010112,,,A*68,F,, imei:123456789012345,04,481.2,F:4.15V,0,139,2689,232,03,2725,0576\n">>,
%%  em_regexp:match(Packet, ?PATTERN_FULL).
