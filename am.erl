-module(am).
-compile(export_all).

init2() ->
    ets:new(proctable,[named_table]).

add2({Name,Type}) ->
    Pid = spawn(?MODULE,fsmServer,[Type,unitialized,[]]),
    ets:insert(proctable,{Name,Pid}).

addAndSet2({Name,Type,State}) ->
    add2({Name,Type}),
    strictSingleSet2(Name,State).    

fsmServer(Type,State,Dependents) ->
    receive
        {From,{set,NewState}} ->
            From ! {am, ok},
            fsmServer(Type,NewState, Dependents);
        {From,{get}} ->
            From ! {am, State},
            fsmServer(Type,State, Dependents);
        {From,{update}} ->
            From ! {am, ok},
            NewState = calcUpdateState(Type,State),
            if
                NewState /= State ->
                    notifyDep(Dependents);
                true ->
                    ok
            end,
            fsmServer(Type, NewState, Dependents);
        {From, {register, DependentName}} ->
            From ! {am, ok},
            Cond = lists:member(DependentName, Dependents),
            NewDependents = if
                (not Cond) ->
                    [DependentName|Dependents];
                true ->
                    Dependents
            end,
            fsmServer(Type, State, NewDependents);
        {From, Anything} ->
            From ! {am, {badarg,Anything}},
            fsmServer(Type, State, Dependents);
        _ ->
            fsmServer(Type, State, Dependents)
    end.

notifyDep(Dependents) ->
    lists:foreach(fun(Name) ->
            update(Name)
        end, Dependents).

calcUpdateState({Type,Depends},State) ->
    UpdateFuncName = dsler:calc_func_name(Type,'update'),
    outerl:UpdateFuncName([State|lists:map(fun(X)->get2(X) end, Depends)]).

%set 1 automata, ignoring all dependencies
strictSingleSet2(Name,State) ->
    [[Pid]] = ets:match(proctable,{Name,'$1'}),
    Pid ! {self(), {set, State}},
    receive
        {am,Any} -> Any
    after
        1000 -> error(timeout,strictSingleSet2)
    end.

registerAm(Name, RegName) ->
    [[Pid]] = ets:match(proctable,{Name,'$1'}),
    Pid ! {self(), {register, RegName}},
    receive
        {am,Any} -> Any
    after
        1000 -> error(timeout,register)
    end.

registerMe(Name, {_Type, Depends}) ->
    lists:foreach(fun(X) ->
                      registerAm(X, Name)
                  end, Depends).

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

test() ->
    update('B'),
    get2('A'),
    timer:sleep(500),
    get2('B').

main2() ->
    InputFile = "file.txt",
    Grammar = grammar,
    OutFile = "outerl",
    {Classes, Ams} = dsler:parse_file(InputFile,Grammar),
    dsler:write_methods(Classes,OutFile),
    init2(),
    lists:foreach(fun({Name,Type,StateExpr}) -> addAndSet2({Name,Type,dsler:eval_expr(StateExpr)}) end,Ams),
    lists:foreach(fun({Name,Type,_StateExpr}) -> registerMe(Name,Type) end,Ams),
    timer:sleep(500),
    Ans = test(),
    ets:delete(proctable),
    Ans.