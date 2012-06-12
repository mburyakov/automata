-module(testserver).
-compile(export_all).



server(Num) when Num>0 ->   
    receive
        {From, Question} ->
            Reply = Question,
            From ! Reply,
            server(Num-1)
    end;
server(0) ->
    undefined.

client(Pid,Num) when Num>0 ->
    Query = io:get_chars("client> ",3),
    Pid ! {self(), Query},
    receive
        Answer -> io:put_chars("answer = " ++ Answer ++ "  "),
        client(Pid,Num-1)
    end;

client(_Pid,0) ->
    [123432453456].

threadInit() ->
    Pid = spawn(?MODULE,server,[5]),
    client(Pid,3),
    exit(Pid, killing).