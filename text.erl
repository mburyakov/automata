-module(automata).
-compile(export_all).

init() ->
    ets:new(automatable,[]).

add(Table,{Name,Type,State}) ->
    ets:insert(Table,{Name,Type,State}).

%set 1 automata, ignoring all dependencies
strictSingleSet(Table,Name,State) ->
    [[Type,_OldState]] = ets:match(Table,{Name,'$1','$2'}),
    ets:insert(Table,{Name,Type,State}).

main() ->
    T = init(),
    add(T,{scb1,manual,1}),
    add(T,{scb2,manual,2}),
    set(T,scb1,3),
    Ans = ets:tab2list(T),
    ets:delete(T),
    Ans.


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
    Pid = spawn(text,server,[1]),
    client(Pid,1),
    exit(Pid, killing).