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
-module(em_teltonika_protocol).
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


-define(MESSAGE_MINIMUM_LENGTH, 12).

-define(CODEC_GH3000, 16#07).
-define(CODEC_FM4X00, 16#08).
-define(CODEC_12, 16#0C).

-define(HEX, "08090000014694820AD8000F0D13A82098D9F5008D00000D00000010060100B3004501F0001503C8000709001FB5000FB60008422E67180000CD038BCE000102C700000000F10000601A014E00000000000000000000014694811C90000F0D13E22098D9CA008E00000D00000010060100B3004501F0001503C8000709001DB50011B60009422E83180000CD038BCE000102C700000000F10000601A014E00000000000000000000014694803230000F0D13A92098D934008F00000A00000010060100B3004501F0001503C8000709001FB50012B6000A422E7A180000CD038BCE000102C700000000F10000601A014E000000000000000000000146947F47D0000F0D13352098D9480091013A0C00000010060100B3004501F0001503C8000709001FB50010B60009422E83180000CD038BCE000102C700000000F10000601A014E000000000000000000000146947E5D70000F0D128C2098D97F0092012A0E00000010060100B3004501F0001503C8000709001FB5000FB60008422E7A180000CD038BCE000102C700000000F10000601A014E000000000000000000000146947D7310000F0D121E2098D975009200000D00000010060100B3004501F0001503C80007090022B5000FB60008422E70180000CD038BCE000102C700000000F10000601A014E000000000000000000000146947C88B0000F0D11BA2098D97A009300000E00000010060100B3004501F0001503C8000709001FB5000FB60008422E70180000CD038BCE000102C700000000F10000601A014E000000000000000000000146947B9E50000F0D12072098D9CA009300000D000000140A010002000300B300B4004501F00050001503C8000709001FB50011B60009422E67180000CD038BCE000103C700000000F10000601A48000000000000000146947AB3F0000F0D11E72098D982009300000D000000140A010002000300B300B4004501F00050001503C8000709001FB50010B60009422E70180000CD038BCE000103C700000000F10000601A480000000000").
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
handle_info({tcp, Socket, Data}, State = #state{ transport =  Transport}) when byte_size(Data) < ?MESSAGE_MINIMUM_LENGTH ->
    Transport:setopts(Socket, ?SOCKET_OPTS),
    {noreply, State, ?TIMEOUT};
handle_info({tcp, Socket, Data = <<Length:16/unsigned-integer, _/binary>>}, State = #state{socket = Socket, transport = Transport}) when Length > 0 ->
    Transport:setopts(Socket, ?SOCKET_OPTS),
    FrameSize = Length  + 2,
    <<Frame:FrameSize/binary, _/binary>> = Data,
    do_process_frame(State, Frame);
handle_info({tcp, Socket, Data = <<_:32/unsigned-integer, Length:32/unsigned-integer, _/binary>>}, State = #state{socket = Socket, transport = Transport}) ->
    Transport:setopts(Socket, ?SOCKET_OPTS),
    FrameSize = Length  + 12,
    <<Frame:FrameSize/binary, _/binary>> = Data,
    do_process_frame(State, Frame);
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

do_process_frame(State = #state{socket = Socket, transport = Transport}, <<Length:16/unsigned-integer, Imei/binary>>) when Length > 0 ->
    case em_data_manager:get_device_by_uid(Imei) of
        {error, _Reason} ->
            em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_inet:resolve(Socket), Imei]),
            Transport:send(Socket, <<16#00>>),
            {stop, normal, State};
        {ok, Object} ->
            em_logger:info("[packet] unit: ip = '~s' accept device with imei = '~s'", [em_inet:resolve(Socket), Imei]),
            em_proc:registry(Object#device.id, self()),
            Transport:send(Socket, <<16#01>>),
            {noreply, State#state{deviceId = Object#device.id}, ?TIMEOUT}
    end;
do_process_frame(State, <<_Zero:32/unsigned-integer, Length:32/unsigned-integer, Body/binary>>) ->
    <<AvlData:Length/binary, _Crc:32/unsigned-integer>> = Body,
    do_process_data(State, AvlData).

do_process_data(State = #state{transport = Transport, socket = Socket, protocol = Protocol, deviceId = DeviceId}, <<Codec:8/unsigned-integer, Count:8/unsigned-integer, Body/binary>>) ->
    %%em_logger:info("TELTONIKA POSITION: DeviceId: ~w, Codec: ~w Count: ~w", [DeviceId, Codec, Count]),
    case parse_data(Codec, Count, Body) of
        {ok, Records} ->
            lists:map(fun(PositionModel) ->
                              %%em_logger:info("[packet] unit: ip = '~s' message: ~w", [em_inet:resolve(Socket), PositionModel]),
                              Position = PositionModel#position{
                                           deviceId = DeviceId,
                                           protocol = atom_to_binary(Protocol, utf8),
                                           attributes = maps:merge(PositionModel#position.attributes, #{
                                                                                            ?KEY_IP => em_inet:resolve(Socket)
                                                                                           })
                                          },
                              em_logger:info("save message => unit: ip = '~s' id = '~w' position: ~w", [em_inet:resolve(Socket), DeviceId, Position]),
                              em_data_manager:create_position(DeviceId, Position)
                      end, Records),
            Transport:send(Socket, <<Count:32/unsigned-integer>>),
            {noreply, State, ?TIMEOUT};
      {error, Reason} ->
            em_logger:info("error => unit: ip = '~s' id = '~w': ~s", [em_inet:resolve(Socket), DeviceId, Reason]),
            Transport:send(Socket, <<0:32/unsigned-integer>>),
            {stop, normal, State}
    end.

parse_data(Codec, Count, Body) ->
    case Codec of
        ?CODEC_12 ->
            {ok, parse_data(Codec, Count, Body, [])};
        ?CODEC_GH3000 ->
            {ok, parse_data(Codec, Count, Body, [])};
        ?CODEC_FM4X00 ->
            {ok, parse_data(Codec, Count, Body, [])};
        _ ->
            {error, <<"Unknown codec">>}
    end.


parse_data(_Codec, 0, _Body, Acc) ->
    Acc;
parse_data(?CODEC_12, Count, Body, Acc) ->
    parse_data(?CODEC_12, Count - 1, Body, Acc);
parse_data(Codec, Count, Body, Acc) ->
    {Position, Bin} = decode_location(Codec, Body),
    %%em_logger:info("TELTONIKA POSITION: ~w", [Position]),
    parse_data(Codec, Count - 1, Bin, [Position | Acc]).


decode_location(?CODEC_GH3000, _Body) ->
    #position{};
decode_location(?CODEC_FM4X00, Body) ->
    <<
      Time:64/unsigned-integer,
      _Priority:8/unsigned-integer,
      Longitude:32/signed-integer,
      Latitude:32/signed-integer,
      Altitude:16/signed-integer,
      Course:16/unsigned-integer,
      Satellites:8/unsigned-integer,
      Speed:16/unsigned-integer,
      Event:8/unsigned-integer,
      _TotalIO:8/unsigned-integer,
      Bin/binary
    >> = Body,

    %%em_logger:info("TELTONIKA TotalIO: ~w", [TotalIO]),
    AttrsBase = #{
      ?KEY_SATELLITES => Satellites,
      ?KEY_EVENT => Event
     },
    {AttrsOne, BinOne} = parse_attrs(Bin, 1),
    {AttrsTwo, BinTwo} = parse_attrs(BinOne, 2),
    {AttrsThree, BinThree} = parse_attrs(BinTwo, 4),
    {AttrsFour, BinFour} = parse_attrs(BinThree, 8),
    Attrs = lists:foldl(fun(Item, Acc) ->
                                maps:merge(Acc, Item)
                        end, #{}, [
                                   AttrsBase,
                                   AttrsOne,
                                   AttrsTwo,
                                   AttrsThree,
                                   AttrsFour
                                  ]),

    Position = #position{
                  deviceTime = parse_time(Time),
                  latitude = parse_coord(Latitude),
                  longitude = parse_coord(Longitude),
                  altitude = Altitude,
                  course = Course,
                  valid = parse_valid(Satellites),
                  speed = parse_speed(Speed),
                  attributes = Attrs
                 },
    {Position, BinFour}.


parse_time(Time) ->
    Time div 1000.

parse_coord(Value) ->
    Value / 10000000.0.

parse_valid(Value) when Value > 0 ->
    true;
parse_valid(_) ->
    false.

parse_speed(Value) ->
    Value * 1000 / 3600.

parse_attrs(<<Cnt:8/unsigned-integer, Body/binary>>, Length) ->
    %%em_logger:info("TELTONIKA Cnt: ~w Length: ~w", [Cnt, Length]),
    decode_parameter(Cnt, Length, Body, #{}).

decode_parameter(0, _, Bin, Acc) ->
    {Acc, Bin};
decode_parameter(Count, Length, <<Id:8/unsigned-integer, Body/binary>>, Acc) ->
    case Id of
        1 ->
            {Value, Bin} = read_value(Length, unsigned, Body),
            %%em_logger:info("TELTONIKA Id: ~w Value: ~w", [Id, Value]),
            Key = list_to_binary(["di" ++ integer_to_list(Id)]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{Key => Value}));
        2 ->
            {Value, Bin} = read_value(Length, unsigned, Body),
            %%em_logger:info("TELTONIKA Id: ~w Value: ~w", [Id, Value]),
            Key = list_to_binary(["di" ++ integer_to_list(Id)]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{Key => Value}));
        3 ->
            {Value, Bin} = read_value(Length, unsigned, Body),
            %%em_logger:info("TELTONIKA Id: ~w Value: ~w", [Id, Value]),
            Key = list_to_binary(["di" ++ integer_to_list(Id)]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{Key => Value}));
        4 ->
            {Value, Bin} = read_value(Length, unsigned, Body),
            %%em_logger:info("TELTONIKA Id: ~w Value: ~w", [Id, Value]),
            Key = list_to_binary(["di" ++ integer_to_list(Id)]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{Key => Value}));
        9 ->
            {Value, Bin} = read_value(Length, unsigned, Body),
            %%em_logger:info("TELTONIKA Id: ~w Value: ~w", [Id, Value]),
            Key = list_to_binary([binary_to_list(?PREFIX_ADC) ++ integer_to_list(1)]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{Key => Value}));
        66 ->
            {Value, Bin} = read_value(Length, unsigned, Body),
            ValueBin = list_to_binary([integer_to_list(Value) ++ "mV"]),
            %%em_logger:info("TELTONIKA KEY_POWER Id: ~w Value: ~s", [Id, ValueBin]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{?KEY_POWER => ValueBin}));
        67 ->
            {Value, Bin} = read_value(Length, unsigned, Body),
            ValueBin = list_to_binary([integer_to_list(Value) ++ "mV"]),
            %%em_logger:info("TELTONIKA KEY_BATTERY Id: ~w Value: ~s", [Id, ValueBin]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{?KEY_BATTERY => ValueBin}));
        70 ->
            {Value, Bin} = read_value(Length, signed, Body),
            %%em_logger:info("TELTONIKA Id: ~w Value: ~w", [Id, Value]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{?KEY_DEVICE_TEMP => Value  * 0.1}));
        72 ->
            {Value, Bin} = read_value(Length, signed, Body),
            %%em_logger:info("TELTONIKA Id: ~w Value: ~w", [Id, Value]),
            Key = list_to_binary([binary_to_list(?PREFIX_TEMP) ++ integer_to_list(1)]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{Key => Value  * 0.1}));
        73 ->
            {Value, Bin} = read_value(Length, signed, Body),
            %%em_logger:info("TELTONIKA Id: ~w Value: ~w", [Id, Value]),
            Key = list_to_binary([binary_to_list(?PREFIX_TEMP) ++ integer_to_list(2)]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{Key => Value  * 0.1}));
        74 ->
            {Value, Bin} = read_value(Length, signed, Body),
            %%em_logger:info("TELTONIKA Id: ~w Value: ~w", [Id, Value]),
            Key = list_to_binary([binary_to_list(?PREFIX_TEMP) ++ integer_to_list(3)]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{Key => Value  * 0.1}));
        78 ->
            {Value, Bin} = read_value(Length, unsigned, Body),
            %%em_logger:info("TELTONIKA Id: ~w Value: ~w", [Id, Value]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{?KEY_RFID => Value}));
        182 ->
            {Value, Bin} = read_value(Length, unsigned, Body),
            %%em_logger:info("TELTONIKA Id: ~w Value: ~w", [Id, Value]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{?KEY_HDOP => Value  * 0.1}));
        _ ->
            {Value, Bin} = read_value(Length, unsigned, Body),
            %%em_logger:info("TELTONIKA UNKINOWN Id: ~w Value: ~w", [Id, Value]),
            Key = list_to_binary([binary_to_list(?PREFIX_IO) ++ integer_to_list(Id)]),
            decode_parameter(Count - 1, Length, Bin, maps:merge(Acc, #{Key => Value}))
    end.

read_value(1, signed, <<Value:8/signed-integer, Body/binary>>) ->
    {Value, Body};
read_value(2, signed, <<Value:16/signed-integer, Body/binary>>) ->
    {Value, Body};
read_value(4, signed, <<Value:32/signed-integer, Body/binary>>) ->
    {Value, Body};
read_value(8, signed, <<Value:64/signed-integer, Body/binary>>) ->
    {Value, Body};
read_value(1, unsigned, <<Value:8/unsigned-integer, Body/binary>>) ->
    {Value, Body};
read_value(2, unsigned, <<Value:16/unsigned-integer, Body/binary>>) ->
    {Value, Body};
read_value(4, unsigned, <<Value:32/unsigned-integer, Body/binary>>) ->
    {Value, Body};
read_value(8, unsigned, <<Value:64/unsigned-integer, Body/binary>>) ->
    {Value, Body}.