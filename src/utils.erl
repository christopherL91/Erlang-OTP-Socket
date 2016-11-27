-module(utils).
-author("Christoher Lillthors").

-export([get_id/2, remove_newlines/1]).

%%  Get unique id's for each child.
get_id(Name, N) ->
    io_lib:format("~s~w", [Name, N]).

remove_newlines(Data) ->
    re:replace(Data, "(^\\s+)|(\\s+$)", "", [global,{return,list}]).

