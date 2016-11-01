%%%-------------------------------------------------------------------
%%% @author Christopher Lillthors <christopher.lillthors@gmail.com>
%%% @copyright (C) 2016, Christopher Lillthors <christopher.lillthors@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 2016-10-28 15:26:24.888408
%%%-------------------------------------------------------------------
-module(chatroom_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

    SupFlags = #{
                    strategy => one_for_one,
                    intensity => 100,
                    period => 3600
                },

    ListenerSup = #{
                        id => chatroom_listener_sup,
                        start => {chatroom_listener_sup, start_link, []},
                        restart => permanent,
                        shutdown => brutal_kill,
                        type => supervisor,
                        modules => [chatroom_listener_sup]
                    },

    WorkerSup = #{
                        id => chatroom_worker_sup,
                        start => {chatroom_worker_sup, start_link, []},
                        restart => permanent,
                        shutdown => brutal_kill,
                        type => supervisor,
                        modules => [chatroom_worker_sup]
                    },


    ChildList = [ListenerSup, WorkerSup],

    {ok, {SupFlags, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
