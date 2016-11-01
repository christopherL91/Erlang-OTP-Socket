%%%-------------------------------------------------------------------
%% @doc fms locker
%% @end
%%%-------------------------------------------------------------------

-module(chatroom_worker_fms).
-author("Christopher Lillthors").

-behaviour(gen_fsm).

%%  API
-export([start_link/0, lock/1, unlock/1]).

%%  gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%%  State functions
-export([locked/2, locked/3, unlocked/2, unlocked/3]).

%%  Application state
-record(state, {
          code
         }).

%%%==========================================================
%%%                     API
%%%==========================================================
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

unlock(Code) ->
    gen_fsm:sync_send_event(?MODULE, {unlock, Code}).

lock(Code) ->
    gen_fsm:sync_send_event(?MODULE, {lock, Code}).

%%%==========================================================
%%%                     GEN_FSM Callbacks
%%%==========================================================

init([]) ->
    logger:info("chatroom_worker_fms:init...~n"),
    InitialState = #state{code = 1234},
    {ok, unlocked, InitialState}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%==========================================================
%%%                     Async State Functions
%%%==========================================================

unlocked(_Event, State) ->
    {next_state, unlocked, State}.

locked(_Event, State) ->
    {next_state, locked, State}.

%%%==========================================================
%%%                     Sync State Functions
%%%==========================================================

unlocked({lock, Code}, _From, State) ->
    case State#state.code =:= Code of
        true ->
            {reply, ok, locked, State};
        false ->
            {reply, {error, wrong_code}, unlocked, State}
    end;

unlocked(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, unlocked, State}.

locked({unlock, Code}, _From, State) ->
    case State#state.code =:= Code of
        true ->
            {reply, ok, unlocked, State};
        false ->
            {reply, {error, wrong_code}, locked, State}
    end;

locked(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, locked, State}.
