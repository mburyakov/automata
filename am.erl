-module(am).
-compile(export_all).

init1() ->
    ets:new(automatable,[named_table]).

init2() ->
    ets:new(proctable,[named_table]).


add1(Table,{Name,Type,State}) ->
    ets:insert(Table,{Name,Type,State}).

add2({Name,Type}) ->
    Pid = spawn(am,fsmServer,[Type,0]),
    ets:insert(proctable,{Name,Pid}).

addAndSet2({Name,Type,State}) ->
    add2({Name,Type}),
    strictSingleSet2(Name,State).

fsmServer(Type,State) ->
    receive
        {From,{set,NewState}} ->
            From ! {am,{State,NewState}},
            fsmServer(Type,NewState);
        {From,{get}} ->
            From ! {am,State},
            fsmServer(Type,State);
        {From,{update}} ->
            NewState = calcState(Type,State),
            From ! {am,{State, NewState}},
            fsmServer(Type, NewState);
        {From, Anything} ->
            From ! {am,{badarg,Anything}},
            fsmServer(Type, State);
        _ ->
            fsmServer(Type, State)
    end.

calcState({Type,Depends},State) ->
    UpdateFuncName = dsler:calc_func_name(Type,'update'),
    %%io:format("~p", lists:map(fun(X)->get2(X) end, Depends)),
    outerl:UpdateFuncName([State|lists:map(fun(X)->get2(X) end, Depends)]).
    %%states:Type(State,Depends).

add1(S) ->
    add1(automatable,S).

%set 1 automata, ignoring all dependencies
strictSingleSet(Table,Name,State) ->
    [[Type,_OldState]] = ets:match(Table,{Name,'$1','$2'}),
    ets:insert(Table,{Name,Type,State}).

strictSingleSet2(Name,State) ->
    [[Pid]] = ets:match(proctable,{Name,'$1'}),
    Pid ! {self(), {set, State}},
    receive
        {am,Any} -> Any
    after
        1000 -> error(timeout,strictSingleSet2)
    end.

update(Name) ->
    [[Pid]] = ets:match(proctable,{Name,'$1'}),
    Pid ! {self(), {update}},
    receive
        {am,Any} -> Any
    after
        1000 -> erlang:error(timeout)
    end.


get2(Name) ->
    [[Pid]] = ets:match(proctable,{Name,'$1'}),
    Pid ! {self(), {get}},
    receive
        {am,Any} -> Any
    after
        1000 -> erlang:error(timeout)
    end.



main() ->
    T = init1(),
    {_Classes, Ams} = dsler:parseFile('file.txt','grammar'),
    lists:foreach(fun(X) -> ?MODULE:add(T,X) end,Ams),
    add1(T,{scb1,manual,1}),
    add1(T,{scb2,manual,2}),
    strictSingleSet(T,scb1,3),
    Ans = ets:tab2list(T),
    ets:delete(T),
    Ans.


main2() ->
    InputFile = "file.txt",
    Grammar = grammar,
    OutFile = "outerl",
    {Classes, Ams} = dsler:parse_file(InputFile,Grammar),
    dsler:write_methods(Classes,OutFile),
    init2(),
    lists:foreach(fun(X) -> ?MODULE:addAndSet2(X) end,Ams),
    update(a),
    Ans = get2(a),
    ets:delete(proctable),
    Ans.
