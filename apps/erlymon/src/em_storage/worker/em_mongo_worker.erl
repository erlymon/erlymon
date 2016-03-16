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

-module(em_mongo_worker).
-author("Sergey Penkovsky <sergey.penkovsky@gmail.com>").

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {conn}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

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
init(Args) ->
  process_flag(trap_exit, true),
  _Host = list_to_binary(proplists:get_value(host, Args)),
  _Port = (proplists:get_value(port, Args)),
  Database = list_to_binary(proplists:get_value(database, Args)),
  _Login = list_to_binary(proplists:get_value(login, Args)),
  _Password = list_to_binary(proplists:get_value(password, Args)),

  %%em_logger:info("init worker args: ~w ~s", [Database, Database]),
  %%em_logger:info("init worker host: ~s, port: ~w database: ~s username: ~s, password: ~s", [Host, Port, Database, Login, Password]),

  {ok, Conn} = mc_worker_api:connect([{database, Database}, {w_mode, safe}]),

  {ok, #state{conn = Conn}}.

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
%%handle_call({squery, Sql}, _From, #state{conn = Conn} = State) ->
%%  {reply, epgsql:squery(Conn, Sql), State};
%%handle_call({equery, Stmt, Params}, _From, #state{conn = Conn} = State) ->
%%  {reply, epgsql:equery(Conn, Stmt, Params), State};
handle_call({insert, Coll, Doc}, _From, #state{conn = Conn} = State) when is_map(Doc) ->
  {reply, mc_worker_api:insert(Conn, Coll, Doc), State};
handle_call({insert, Coll, Docs}, _From, #state{conn = Conn} = State) when is_list(Docs) ->
  {reply, mc_worker_api:insert(Conn, Coll, Docs), State};

handle_call({update, Coll, Selector, Doc}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:update(Conn, Coll, Selector, Doc), State};
handle_call({update, Coll, Selector, Doc, Upsert}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:update(Conn, Coll, Selector, Doc, Upsert), State};
handle_call({update, Coll, Selector, Doc, Upsert, MultiUpdate}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:update(Conn, Coll, Selector, Doc, Upsert, MultiUpdate), State};


handle_call({delete, Coll, Selector}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:delete(Conn, Coll, Selector), State};

handle_call({delete_one, Coll, Selector}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:delete_one(Conn, Coll, Selector), State};


handle_call({find, Coll, Selector}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:find(Conn, Coll, Selector), State};
handle_call({find, Coll, Selector, Projector}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:find(Conn, Coll, Selector, Projector), State};
handle_call({find, Coll, Selector, Projector, Skip}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:find(Conn, Coll, Selector, Projector, Skip), State};
handle_call({find, Coll, Selector, Projector, Skip, BatchSize}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:find(Conn, Coll, Selector, Projector, Skip, BatchSize), State};

handle_call({find_one, Coll, Selector}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:find_one(Conn, Coll, Selector), State};
%%handle_call({find_one, Coll, Selector, Projector}, _From, #state{conn = Conn} = State) ->
%%  {reply, mc_worker_api:find_one(Conn, Coll, Selector, Projector), State};
handle_call({find_one, Coll, Selector, Projector}, _From, #state{conn = Conn} = State) ->
  %%em_logger:debug("################### ~w", [{find_one, Coll, Selector, Projector}]),
  {reply, mc_worker_api:find_one(Conn, Coll, Selector, Projector), State};
handle_call({find_one, Coll, Selector, Projector, Skip}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:find_one(Conn, Coll, Selector, Projector, Skip), State};

handle_call({count, Coll, Selector}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:count(Conn, Coll, Selector), State};
handle_call({count, Coll, Selector, Limit}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:count(Conn, Coll, Selector, Limit), State};
handle_call({ensure_index, Coll, Spec}, _From, #state{conn = Conn} = State) ->
  {reply, mc_worker_api:ensure_index(Conn, Coll, Spec), State};

handle_call(_Request, _From, State) ->
  {reply, {error, <<"Invalid em_mongo_worker call">>}, State}.

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
  mc_worker_api:disconnect(Conn),
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

