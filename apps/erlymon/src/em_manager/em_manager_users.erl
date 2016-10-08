%%%-------------------------------------------------------------------
%%% @author sergey
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Окт. 2016 6:37 AM
%%%-------------------------------------------------------------------
-module(em_manager_users).
-author("sergey").

-behaviour(gen_server).

-include("em_records.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0]).

-export([
         get/0,
         check/2
        ]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {cache}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(get() -> {ok, [Rec :: #user{}]} | {error, string()}).
get() ->
    gen_server:call(?SERVER, {get}).


-spec(check(Email :: string(), Password :: string()) -> {ok, Rec :: #user{}} | {error, string()}).
check(Email, Password) ->
    gen_server:call(?SERVER, {check, Email, Password}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
             {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    em_logger:info("Init users manager"),
    Cache = ets:new(users, [set, private, {keypos, 1}]),
    {ok, Users} = em_storage:get_users(),
    lists:foreach(fun(User) -> ets:insert(Cache, User) end, Users),
    em_logger:info("Loaded ~w user(s)", [length(ets:tab2list(Cache))]),
    {ok, #state{cache = Cache}}.

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
handle_call({get}, _From, State) ->
    do_get_users(State);
handle_call({check, Email, Password}, _From, State) ->
    do_check_user(State, Email, Password);
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
terminate(_Reason, #state{cache = Cache}) ->
    ets:delete(Cache),
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

do_get_users(State = #state{cache = Cache}) ->
    {reply, ets:tab2list(Cache), State}.

%% em_manager_users:check(<<"demo">>, <<"demo">>).
do_check_user(State = #state{cache = Cache}, Email, Password) ->
    CheckPass = fun(CurrPassword, User = #user{hashPassword = HashPassword}) ->
                        case CurrPassword =:= HashPassword of
                            true -> {ok, User};
                            false -> {error, <<"Access is denied">>}
                        end
                end,
    Match = ets:fun2ms(fun(User = #user{email = SearchEmail}) when SearchEmail =:= Email -> User end),
    case ets:select(Cache, Match) of
        [] ->
            {reply, {error, <<"Access is denied">>}, State};
        [Item|_] ->
            {reply, CheckPass(em_password:hash(Password), Item), State}
    end.