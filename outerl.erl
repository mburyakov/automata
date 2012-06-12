-module(outerl).
-compile(export_all).


'manual:init'([This]) ->
    0.

'manual:update'([This]) ->
    This.

'base:init'([This,Arg]) ->
    0.

'base:update'([This,Arg]) ->
    'base:the_method'([This,Arg], Arg).

'base:the_method'([This,Arg],A) ->
    Arg + A.

'counter:init'([This]) ->
    0.

'counter:update'([This]) ->
    This + 1.

'linear:init'([This,Arg]) ->
    0.

'linear:update'([This,Arg]) ->
    Arg - 1.

'switch:init'([This]) ->
    0.

'switch:update'([This]) ->
    This.

'double:init'([This,Arg1,Arg2,Sw]) ->
    0.

'double:update'([This,Arg1,Arg2,Sw]) ->
    if
    Sw ->
        'linear:update'([This,Arg1]);
    true ->
        'linear:update'([This,Arg2])
end.

'next:init'([This,Arg]) ->
    undefined.

'next:update'([This,Arg]) ->
    Arg.

'cycle:init'([This,Arg]) ->
    30000.

'cycle:update'([This,Arg]) ->
    if
    Arg == 0 ->
        0;
    true ->
        Arg - 1
end.
