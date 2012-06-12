-module(outerl).
-compile(export_all).


'manual:update'([This]) ->
    This.

'base:update'([This,Arg]) ->
    'base:the_method'([This,Arg], Arg).

'base:the_method'([This,Arg],A) ->
    Arg + A.

'counter:update'([This]) ->
    This + 1.

'linear:update'([This,Arg]) ->
    Arg - 1.

'switch:update'([This]) ->
    This.

'double:update'([This,Arg1,Arg2,Sw]) ->
    if
    Sw ->
        'linear:update'([This,Arg1]);
    true ->
        'linear:update'([This,Arg2])
end.

'next:update'([This,Arg]) ->
    Arg.

'cycle:update'([This,Arg]) ->
    if
    Arg == 0 ->
        0;
    true ->
        Arg - 1
end.
