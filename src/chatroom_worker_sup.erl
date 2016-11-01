%%%-------------------------------------------------------------------
%%% @author Christopher Lillthors <christopher.lillthors@gmail.com>
%%% @copyright (C) 2016, Christopher Lillthors <christopher.lillthors@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 2016-10-28 15:39:50.815884
%%%------------------------------------------------------------------
-module(chatroom_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

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

%%--------------------------------------------------------------------
%% @doc
%% Starts a child
%%
%% @spec start_child(Socket) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_child(Socket) ->
    case supervisor:start_child(?MODULE, [Socket]) of
        {ok, Pid} ->
            gen_tcp:controlling_process(Socket, Pid),
            {ok, Pid};
        Error ->
            Error
    end.

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
                    strategy => simple_one_for_one,
                    intensity => 100,
                    period => 3600
                },

    Worker = #{
                id => chatroom_worker_gen,
                start => {chatroom_worker_gen, start_link, []},
                restart => temporary,
                shutdown => brutal_kill,
                type => worker,
                modules => [chatroom_worker_gen]
            },

    ChildList = [Worker],

    {ok, {SupFlags, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
