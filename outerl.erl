-module(outerl).
-compile(export_all).


'manual:update'([This]) ->
    This.

'base:update'([This,Arg]) ->
    'base:the_method'([This,Arg], Arg).

'base:the_method'([This,Arg],A) ->
    Arg + A.

'linear:update'([This,Arg1,Arg2]) ->
    'manual:update'([This]), 'base:the_method'([This,Arg1], Arg2).
