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
-module(em_gt06_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("em_hardware.hrl").
-include("em_records.hrl").

-ifdef(TEST).
-compile(export_all).
-endif. % EXPORT_ALL

%% API
-export([start_link/4]).
%%-export([decode_test/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, infinity).

-define(SERVER, ?MODULE).
-define(SOCKET_OPTS, [{active, once}, {packet, raw}]).


-define(MSG_LOGIN, 16#01).
-define(MSG_GPS_LBS_1, 16#12).
-define(MSG_STATUS,16#13).

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
handle_info({tcp, _, <<67, 78, 88, 78, 0, 0, 0, 1, 0, 0, 4, 0, 27, 0, 0, 0, 77, 10>>}, State = #state{socket = Socket, transport = Transport}) ->
    Transport:setopts(Socket, ?SOCKET_OPTS),
    {noreply, State, ?TIMEOUT};
handle_info({tcp, Socket, Data = <<16#78, 16#78, Length:8/unsigned-integer, _/binary>>}, State = #state{socket = Socket, transport = Transport}) ->
  Transport:setopts(Socket, ?SOCKET_OPTS),
  em_logger:info("DATA: ~w", [Data]),
  em_logger:info("LENGTH: ~w", [Length]),
  FrameSize = 5 + Length,
  <<FrameData:FrameSize/binary, _/binary>> = Data,
  case FrameData of
    <<16#78, 16#78, _:8/unsigned-integer, _:Length/binary, 16#0D, 16#0A>> ->
      em_logger:info("FrameData: ~w", [FrameData]),
      em_logger:info("FrameSize: ~w", [FrameSize]),
      do_process_frame(State, FrameData);
    _ ->
      {stop, normal, State}
  end;
handle_info({tcp, Socket, Data}, State = #state{socket = Socket, transport = Transport}) ->
    Transport:setopts(Socket, ?SOCKET_OPTS),
    em_logger:info("UNKNOWN SIZE: ~w DATA: ~w", [byte_size(Data), Data]),
    {stop, normal, State};
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
do_execute_command(State = #state{transport = _Transport, socket = _Socket}, _Command) ->
    {noreply, State, ?TIMEOUT}.
%% LOGIN_DATA: <<120,120,13,1,3,88,137,144,84,153,83,34,0,7,244,186,13,10>>
%% STATUS_DATA: <120,120,10,19,4,6,4,0,2,0,39,62,230,13,10>>
%% GPS_DATA: <<120,120,31,18,17,3,31,12,50,14,201,3,15,128,216,8,72,56,16,0,84,216,1,148,10,1,3,0,71,81,0,40,94,232,13,10>>
do_process_frame(State = #state{socket = Socket, deviceId = DeviceId, protocol = Protocol, lastPosition = LastPosition}, Data) ->
  case decode(LastPosition, Data) of
    {ok, {?MSG_LOGIN, Index, Imei}} ->
      case em_data_manager:get_device_by_uid(Imei) of
        {error, _Reason} ->
          em_logger:info("[gt06:packet] unit: ip = '~s' unknown device with imei = '~s'", [em_inet:resolve(Socket), Imei]),
          {stop, normal, State};
        {ok, Device} ->
          em_logger:info("[gt06:packet] unit: ip = '~s' accept device with imei = '~s'", [em_inet:resolve(Socket), Imei]),
          em_proc:registry(DeviceId, self()),
          do_send_response(State, ?MSG_LOGIN, Index),
          case em_data_manager:get_last_position(Device#device.positionId, Device#device.id) of
            {ok, Position} ->
              {noreply, State#state{deviceId = Device#device.id, lastPosition = Position}, ?TIMEOUT};
            _ ->
              {noreply, State#state{deviceId = Device#device.id}, ?TIMEOUT}
          end
      end;
    {ok, {?MSG_STATUS, Index, PositionModel}} ->
      em_logger:info("STATUS DATA: ~w Index: ~w", [PositionModel, Index]),
      %%em_logger:info("[packet] unit: ip = '~s' message: ~w", [em_inet:resolve(Socket), PositionModel]),
      Position = PositionModel#position{
        deviceId = DeviceId,
        deviceTime = em_helper_time:timestamp(),
        protocol = atom_to_binary(Protocol, utf8),
        attributes = maps:merge(PositionModel#position.attributes, #{
          ?KEY_IP => em_inet:resolve(Socket)
        })
      },
      em_logger:info("save message => unit: ip = '~s' id = '~w' position: ~w", [em_inet:resolve(Socket), DeviceId, Position]),
      em_data_manager:create_position(DeviceId, Position),
      do_send_response(State, ?MSG_STATUS, Index),
      {noreply, State#state{lastPosition = Position}, ?TIMEOUT};
    {ok, {?MSG_GPS_LBS_1, Index, PositionModel}} ->
      em_logger:info("GPS DATA: ~w Index: ~w", [PositionModel, Index]),
      %%em_logger:info("[packet] unit: ip = '~s' message: ~w", [em_inet:resolve(Socket), PositionModel]),
      Position = PositionModel#position{
        deviceId = DeviceId,
        protocol = atom_to_binary(Protocol, utf8),
        attributes = maps:merge(PositionModel#position.attributes, #{
          ?KEY_IP => em_inet:resolve(Socket)
        })
      },
      em_logger:info("save message => unit: ip = '~s' id = '~w' position: ~w", [em_inet:resolve(Socket), DeviceId, Position]),
      em_data_manager:create_position(DeviceId, Position),
      do_send_response(State, ?MSG_GPS_LBS_1, Index),
      {noreply, State#state{lastPosition = Position}, ?TIMEOUT};
    {error, Reason} ->
      em_logger:info("[gt06:packet] unit ip: '~s' message: '~s'", [em_inet:resolve(Socket), Reason]),
      {stop, normal, State}
  end.


do_send_response(#state{transport = Transport, socket = Socket}, Type, Index) ->
  Body = <<
    5:8/unsigned-integer,
    Type:8/unsigned-integer,
    Index:16/unsigned-integer
  >>,
  Crc = em_checksum:crc16(crc16_x25, Body),
  Packet = <<
    16#78,
    16#78,
    Body/binary,
    Crc:16/unsigned-integer,
    16#0D,
    16#0A
  >>,
  Transport:send(Socket, Packet).


decode(_, <<16#78, 16#78, _Length:8/unsigned-integer, ?MSG_LOGIN:8/unsigned-integer, ImeiBin:8/binary, Index:16/unsigned-integer, _Crc:16/unsigned-integer, 16#0D, 16#0A>>) ->
  {ok, {?MSG_LOGIN, Index, parse_imei(ImeiBin)}};
decode(Position, <<16#78, 16#78, _Length:8/unsigned-integer, ?MSG_STATUS:8/unsigned-integer, _StatusBody:5/binary, Index:16/unsigned-integer, _Crc:16/unsigned-integer, 16#0D, 16#0A>>) ->
  {ok, {?MSG_STATUS, Index, decode_status(Position, _StatusBody)}};
decode(Position, <<16#78, 16#78, _Length:8/unsigned-integer, ?MSG_GPS_LBS_1:8/unsigned-integer, _GpsBody:18/binary, _LbsBody:8/binary, Index:16/unsigned-integer, _Crc:16/unsigned-integer, 16#0D, 16#0A>>) ->
  {ok, {?MSG_GPS_LBS_1, Index, decode_gps(Position, _GpsBody)}};
decode(_, _) ->
  {error, <<"Decoder error">>}.

decode_gps(undefined, <<
  Year:8/unsigned-integer,
  Month:8/unsigned-integer,
  Day:8/unsigned-integer,
  Hour:8/unsigned-integer,
  Minute:8/unsigned-integer,
  Second:8/unsigned-integer,
  _:4/bits,
  Satellites:4/unsigned-integer,
  LatitudeInt:32/unsigned-integer,
  LongitudeInt:32/unsigned-integer,
  Speed:8/unsigned-integer,

  _:3/bits,
  Valid:1,
  EW:1,
  SN:1,
  CourseFirstPart:2/bits,
  CourseSecPart:8/bits,
  _/binary
>>) ->

  em_logger:info("### !! CourseSecPart: ~w CourseFirstPart: ~w Valid: ~w EW:~w SN: ~w", [CourseSecPart, CourseFirstPart, Valid, EW, SN]),
  <<Course:10/unsigned-integer>> = <<CourseFirstPart:2/bits, CourseSecPart:8/bits>>,
  em_logger:info("### !! LatitudeInt: ~w, LongitudeInt: ~w Speed: ~w Valid: ~w EW:~w SN: ~w Course: ~w", [LatitudeInt, LongitudeInt, Speed, Valid, EW, SN, Course]),
  #position{
    deviceTime = parse_date(Year, Month, Day, Hour, Minute, Second),
    latitude = parse_latitude(LatitudeInt, SN),
    longitude = parse_longitude(LongitudeInt, EW),
    speed = Speed,
    course = Course,
    valid = parse_valid(Valid),
    attributes = #{
      ?KEY_SATELLITES => Satellites
    }
  };
decode_gps(Position, <<
  Year:8/unsigned-integer,
  Month:8/unsigned-integer,
  Day:8/unsigned-integer,
  Hour:8/unsigned-integer,
  Minute:8/unsigned-integer,
  Second:8/unsigned-integer,
  _:4/bits,
  Satellites:4/unsigned-integer,
  LatitudeInt:32/unsigned-integer,
  LongitudeInt:32/unsigned-integer,
  Speed:8/unsigned-integer,

  _:3/bits,
  Valid:1,
  EW:1,
  SN:1,
  CourseFirstPart:2/bits,
  CourseSecPart:8/bits,
  _/binary
>>) ->

  em_logger:info("### !! CourseSecPart: ~w CourseFirstPart: ~w Valid: ~w EW:~w SN: ~w", [CourseSecPart, CourseFirstPart, Valid, EW, SN]),
  <<Course:10/unsigned-integer>> = <<CourseFirstPart:2/bits, CourseSecPart:8/bits>>,
  em_logger:info("### !! LatitudeInt: ~w, LongitudeInt: ~w Speed: ~w Valid: ~w EW:~w SN: ~w Course: ~w", [LatitudeInt, LongitudeInt, Speed, Valid, EW, SN, Course]),
  Position#position{
    deviceTime = parse_date(Year, Month, Day, Hour, Minute, Second),
    latitude = parse_latitude(LatitudeInt, SN),
    longitude = parse_longitude(LongitudeInt, EW),
    speed = Speed,
    course = Course,
    valid = parse_valid(Valid),
    attributes = #{
      ?KEY_SATELLITES => Satellites
    }
  }.

decode_status(undefined, <<
  Ignition:1,
  _:7/bits,
  Battery:8/unsigned-integer,
  RSSI:8/unsigned-integer,
  Alarm:8/unsigned-integer,
  _:8/bits
>>) ->
  #position{
    attributes = #{
      ?KEY_IGNITION => Ignition,
      ?KEY_BATTERY => Battery,
      ?KEY_RSSI => RSSI,
      ?KEY_ALARM => decode_alarm(Alarm)
    }
  };
decode_status(LastPosition, <<
  Ignition:1,
  _:7/bits,
  Battery:8/unsigned-integer,
  RSSI:8/unsigned-integer,
  Alarm:8/unsigned-integer,
  _:8/bits
>>) ->
  LastPosition#position{
    attributes = maps:merge(LastPosition#position.attributes, #{
      ?KEY_IGNITION => Ignition,
      ?KEY_BATTERY => Battery,
      ?KEY_RSSI => RSSI,
      ?KEY_ALARM => decode_alarm(Alarm)
    })
  }.

decode_alarm(16#01) -> ?ALARM_SOS;
decode_alarm(16#02) -> ?ALARM_POWER_CUT;
decode_alarm(16#03) -> ?ALARM_VIBRATION;
decode_alarm(16#04) -> ?ALARM_GEOFENCE_ENTER;
decode_alarm(16#05) -> ?ALARM_GEOFENCE_EXIT;
decode_alarm(16#06) -> ?ALARM_OVERSPEED;
decode_alarm(16#09) -> ?ALARM_VIBRATION;
decode_alarm(16#0E) -> ?ALARM_LOW_BATTERY;
decode_alarm(16#0F) -> ?ALARM_LOW_BATTERY;
decode_alarm(16#11) -> ?ALARM_POWER_OFF;
decode_alarm(_) -> null.

parse_valid(1) -> true;
parse_valid(0) -> false.


parse_latitude(Val, 0) ->
  -1 * (Val / 60.0 / 30000.0);
parse_latitude(Val, 1) ->
  Val / 60.0 / 30000.0.

parse_longitude(Val, 1) ->
  -1 * (Val / 60.0 / 30000.0);
parse_longitude(Val, 0) ->
  Val / 60.0 / 30000.0.

parse_imei(Bin) ->
  list_to_binary(lists:flatten([io_lib:format("~.16.0B", [X]) ||
    X <- binary_to_list(Bin)])).

parse_date(Year, Month, Day, Hour, Minute, Second) ->
  Date = {
    {
      Year + 2000,
      Month,
      Day
    },
    {
      Hour,
      Minute,
      Second
    }
  },
  em_helper_time:datetime_to_utc(Date).

%%decode_test() ->
%%  Sample = {ok,{?MSG_GPS_LBS_1,40,{position,0,<<>>,<<>>,0,1490964614,0,0,false,true,-28.526484444444446,77.19481777777777,0.0,0,216,<<>>,#{<<115,97,116>> => 9}}}},
%%  %%Res = decode(#position{}, <<16#78, 16#78, 16#1F, 16#12, 16#0B, 16#08, 16#1D, 16#11, 16#2E, 16#10, 16#CC, 16#02, 16#7A, 16#C7, 16#EB, 16#0C, 16#46, 16#58, 16#49, 16#00, 16#14, 16#8F, 16#01, 16#CC, 16#00, 16#28, 16#7D, 16#00, 16#1F, 16#B8, 16#00, 16#03, 16#80, 16#81, 16#0D, 16#0A>>),
%%  Res = decode(#position{}, <<120,120,31,18,17,3,31,12,50,14,201,3,15,128,216,8,72,56,16,0,84,216,1,148,10,1,3,0,71,81,0,40,94,232,13,10>>),
%%  em_logger:info("RES: ~w", [Res]),
%%  Res =:= Sample.