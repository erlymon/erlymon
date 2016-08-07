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
-module(em_wialon_protocol).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(ranch_protocol).

-include("em_records.hrl").

%% API
-export([start_link/4]).
-export([init/4]).
-export([test/0]).

-define(Date, <<"(\\d{2})(\\d{2})(\\d{2});">>). %% date (ddmmyy)
-define(Time, <<"(\\d{2})(\\d{2})(\\d{2});">>). %% time
-define(Latitude, <<"(\\d{2})(\\d{2}.\\d+);">>).%% latitude
-define(NS, <<"([NS]);">>). %%
-define(Longitude, <<"(\\d{3})(\\d{2}.\\d+);">>). %% longitude
-define(EW, <<"([EW]);">>). %% Calling number
-define(Speed, <<"(\\d+.?\\d*)?;">>). %% Speed
-define(Course, <<"(\\d+.?\\d*)?;">>). %% Course
-define(Altitude, <<"(?:NA|(\\d+.?\\d*));">>).%% Altitude
-define(Satellites, <<"(?:NA|(\\d+))">>). %% Satellites
-define(GroupBegin, <<";">>). %%
-define(Hdop, <<"(?:NA|(\\d+.?\\d*));">>). %% Hdop
-define(Inputs, <<"(?:NA|(\\d+));">>). %% Inputs
-define(Outputs, <<"(?:NA|(\\d+));">>). %% Outputs
-define(Adc, <<"(?:NA|([^;]*));">>). % Adc
-define(IButton, <<"(?:NA|([^;]*));">>). % IButton
-define(Params, <<"(?:NA|(.*))">>). % Params
-define(GroupEnd, <<"?">>).

-define(PATTERN,
  <<
    ?Date/binary,
    ?Time/binary,
    ?Latitude/binary,
    ?NS/binary,
    ?Longitude/binary,
    ?EW/binary,
    ?Speed/binary,
    ?Course/binary,
    ?Altitude/binary,
    ?Satellites/binary,
    ?GroupBegin/binary,
    ?Hdop/binary,
    ?Inputs/binary,
    ?Outputs/binary,
    ?Adc/binary,
    ?IButton/binary,
    ?Params/binary,
    ?GroupEnd/binary
  >>
).

-record(state, {protocol, transport, socket, timeout, device, pattern}).

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
  ok = ranch:accept_ack(Ref),
  Protocol = proplists:get_value(protocol, Opts),
  {ok, Pattern} = re:compile(?PATTERN),
  loop(#state{protocol = Protocol, socket = Socket, transport = Transport, pattern = Pattern}).


loop(State = #state{protocol = Protocol, socket = Socket, transport = Transport, pattern = Pattern}) ->
  case Transport:recv(Socket, 0, 5000) of
    {ok, Data} ->
      em_logger:info("[packet] unit: ip = '~s' data: ~s", [em_hardware:resolve(Socket), Data]),
      [_, Head, Body] = binary:split(Data, [<<"#">>], [global]),
      case parse(Head, Body, Pattern) of
        {auth, Imei, Password} ->
          em_logger:info("[packet] unit: ip = '~s' imei: ~s password: ~s", [em_hardware:resolve(Socket), Imei, Password]),
          case em_data_manager:get_device_by_uid(Imei) of
            null ->
              em_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [em_hardware:resolve(Socket), Imei]),
              send_packet(State, <<"#AL#0">>),
              Transport:close(Socket);
            Object ->
              send_packet(State, <<"#AL#1">>),
              loop(State#state{device = Object})
          end
      end,
      loop(State);
    _ ->
      Transport:close(Socket)
  end.

parse(<<"L">>, Data, _) ->
  case binary:split(Data, [<<";">>], []) of
    [Imei, Password] -> {auth, Imei, fix_password(Password)};
    [Imei] -> {auth, Imei, <<>>}
  end;
parse(<<"D">>, Data, Pattern) ->
  case data_match(Data, Pattern) of
    [_, Date, Time, Latitude, NS, Longitude, EW, Speed, Course, Altitude, Satellites, Hdop, Inputs, Outputs, Adc, IButton, Params | _] ->
      Position = #position{

      },
      {position, Position};
    Reason ->
      {error, Reason}
  end.

fix_password(<<"NA">>) -> <<>>;
fix_password(Val) -> Val.


send_packet(#state{socket = Socket, transport = Transport}, Bin) ->
    Transport:send(Socket, Bin).

% echo "39. wialon"
% (echo -n -e "#D#030816;142342;5354.33140;N;02730.14582;E;0;0;0;NA;NA;NA;NA;;NA;NA\r\n";) | nc -v localhost 5003
do_parse(Data, Pattern) ->
  case data_match(Data, Pattern) of
    [_, Date, Time, Latitude, NS, Longitude, EW, Speed, Course, Altitude, Satellites, Hdop, Inputs, Outputs, Adc, IButton, Params | _] ->
      Message = #position{

      };
    _ ->
      {error}
  end.


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

data_match(Data, Pattern) ->
  {match, List} = re:run(Data, Pattern),
  lists:reverse(lists:foldl(fun(Param, Res) ->
    [read_param(Param, Data) | Res]
                            end, [], List)).

read_param({-1, 0}, _) ->
  void;
read_param({Pos, Len}, Data) ->
  binary:part(Data, Pos, Len).

test() ->
  Packet = <<"#D#030816;142342;5354.33140;N;02730.14582;E;0;0;0;NA;NA;NA;NA;;NA;NA\r\n">>,
  {ok, Pattern} = re:compile(?PATTERN),
  parse(<<"D">>, Packet, Pattern).

