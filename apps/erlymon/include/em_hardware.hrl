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
-ifndef(HARDWARE_HRL).
-define(HARDWARE_HRL, true).

-define(TRASH, <<67,78,88,78,0,0,0,1,0,0,4,0,27,0,0,0,77,10,0,0,188,177,167,177,104,111,115,116,58,58,102,101,97,116,117,114,101,115,61,99,109,100,44,115,104,101,108,108,95,118,50>>).


-record(state, {
  protocol :: atom(),
  transport :: any(),
  socket :: any(),
  timeout :: integer() | atom(),
  deviceId = 0 :: integer()
}).

-endif. % HTTP_HRL
