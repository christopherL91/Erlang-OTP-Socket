%%%-------------------------------------------------------------------
%%% @author Christopher Lillthors <christopher.lillthors@gmail.com>
%%% @copyright (C) 2016, Christopher Lillthors <christopher.lillthors@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 2016-10-28 15:39:05.026469
%%%-------------------------------------------------------------------
-module(chatroom_listener_sup).

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
    Port = application:get_env(chatroom, port, 0),
    lager:info("Server is listening on: 0.0.0.0:~p~n", [Port]),
    NumWorkers = 3,

    Options = [{active, false}, {exit_on_close, true}, {reuseaddr, true}],
    {ok, LSocket} = gen_tcp:listen(Port, Options),

    SupFlags = #{
                    strategy => one_for_one,
                    intensity => 100,
                    period => 3600
                },

    ChildList = [#{
        id => utils:get_id("chatroom_accept_fsm", N),
        start => {chatroom_accept_fsm, start_link, [LSocket]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [chatroom_accept_fsm]
     } || N <- lists:seq(1, NumWorkers)],

    {ok, {SupFlags, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
