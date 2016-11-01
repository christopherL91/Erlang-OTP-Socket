%%%-------------------------------------------------------------------
%%% @author Christopher Lillthors <christopher.lillthors@gmail.com>
%%% @copyright (C) 2016, Christopher Lillthors <christopher.lillthors@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 2016-10-28 15:40:51.093590
%%%-------------------------------------------------------------------
-module(chatroom_worker_gen).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          socket,
          fms
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

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
init([Socket]) ->
    process_flag(trap_exit, true),
    gen_server:cast(self(), startup),
    {ok, #state{socket = Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(startup, State) ->
    E = chatroom_worker_fms:start_link(),
    io:format("New PID: ~p~n", [E]),
    {noreply, State};
    %{noreply, State#state{fms = Pid}};

handle_cast(_Msg, State) ->
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

handle_info({tcp, Socket, Data}, State) ->
    io:format("~p~n", [Data]),
    send(Socket, Data),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    io:format("Closed socket...~n"),
    {stop, normal, State};

handle_info({tcp_error, _Socket}, State) ->
    io:format("TCP error...~n"),
    {stop, normal, State};

handle_info(Info, State) ->
    io:format("unexpected: ~p~n", [Info]),
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
terminate(_Reason, #state{socket = Socket}) ->
    io:format("Terminated...~n"),
    gen_tcp:close(Socket),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send data using Socket
%%
%% @spec send(Socket, Data) -> ok,
%% @end
%%--------------------------------------------------------------------
send(Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
        {error, timeout} ->
            exit("Send timeout");
        {error, Reason} ->
            Str = io_lib:format("Exit reason: ~s", [Reason]),
            exit(Str);
        ok ->
            inet:setopts(Socket, [{active, once}, {packet, line}])
    end.
