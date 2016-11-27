-module(chatroom_worker_fms).
-behaviour(gen_statem).

-export([start_link/1]).
-export([init/1,callback_mode/0,terminate/3,code_change/4]).
-export([locked/3,open/3]).

start_link(Code) ->
    gen_statem:start_link(?MODULE, Code, []).

init(Code) ->
    do_lock(),
    Data = #{code => Code, remaining => Code},
    {ok,locked,Data}.

callback_mode() ->
    state_functions.

locked(
  cast, {button,Digit},
  #{code := Code, remaining := Remaining} = Data) ->
    io:format("Code: ~p Remaining ~p~n", [Code, Remaining]),
    case Remaining of
        [Digit] ->
	    do_unlock(),
            {next_state,open, Data#{remaining := Code}, 10000};
        [Digit|Rest] -> % Incomplete
            {next_state,locked, Data#{remaining := Rest}};
        _Wrong ->
            {next_state,locked, Data#{remaining := Code}}
    end.

open(timeout, _,  Data) ->
    do_lock(),
    {next_state,locked,Data};
open(cast, {button,_}, Data) ->
    do_lock(),
    {next_state,locked,Data}.

do_lock() ->
    io:format("Lock~n", []).
do_unlock() ->
    io:format("Unlock~n", []).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
