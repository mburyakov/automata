-module(am).
-compile(export_all).

init() ->
    ets:new(proctable,[named_table]).

addAm({Name,Type}) ->
    Pid = spawn(?MODULE,fsmServer,[Name, Type,'$uninitialized',[]]),
    ets:insert(proctable,{Name,Pid}).

addAndSetAm({Name,Type,State}) ->
    addAm({Name,Type}),
    strictSingleSetAm(Name,State).
    
initSpawn(Type, State) ->
    io:format("initSpawn ~p\n",[Type]),
    Parent = self(),
    spawn(fun() ->
              Parent ! {self(), {setinit, calcInitState(Type,State)}},
              receive
                  {am,Any} -> Any
              after
                  1000 -> error({timeout, thinkSpawn})
              end
          end).
          
updateSpawn(Type, State) ->
    Parent = self(),
    spawn(fun() ->
              Parent ! {self(), {setupdate, calcUpdateState(Type,State)}},
              receive
                  {am,Any} -> Any
              after
                  1000 -> error({timeout, thinkSpawn})
              end
          end).          

fsmServer(Name, Type, State,Dependents) ->
    %io:format("created ~p (instance of ~p)\n",[self(),Type]),
    receive
        {From, {setupdate, NewState}} ->        
            if
                (NewState /= State) ->
                    notifyDep(Dependents);
                true ->
                    ok
            end,
            From ! {am, ok},
            fsmServer(Name, Type, NewState, Dependents);
        {From, {setinit, NewState}} ->            
            From ! {am, ok},
            {'$initializing', Waiters} = State,
            lists:map(fun(Client) -> Client ! {am, ok} end, Waiters),
            fsmServer(Name, Type, NewState, Dependents);
        {From, {get}} ->
            From ! {am, State},
            fsmServer(Name, Type, State, Dependents);
        {From, {update}} ->            
            updateSpawn(Type, State),
            From ! {am, ok},
            fsmServer(Name, Type, State, Dependents);
        {From, {init}} ->           
            io:format("init received"),
            case State of
                '$uninitialized' ->
                    initSpawn(Type, State),
                    fsmServer(Name, Type, {'$initializing', [From]}, Dependents);
                {'$initializing', Waiters} ->
                    fsmServer(Name, Type, {'$initializing', [From|Waiters]}, Dependents);
                _ ->
                    From ! {am, ok},
                    fsmServer(Name, Type, State, Dependents)
            end;            
        {From, {register, DependentName}} ->
            Cond = lists:member(DependentName, Dependents),
            NewDependents = if
                (not Cond) ->
                    [DependentName|Dependents];
                true ->
                    Dependents
            end,
            From ! {am, ok},
            fsmServer(Name, Type, State, NewDependents);
        {From, Anything} ->
            From ! {am, {badarg, Anything}},
            fsmServer(Name, Type, State, Dependents);
        _ ->
            fsmServer(Name, Type, State, Dependents)
    end.

notifyDep(Dependents) ->
    lists:foreach(fun(Name) ->
                      updateAm(Name)
                  end, Dependents).

calcUpdateState({Type,Depends},State) ->
    UpdateFuncName = dsler:calc_func_name(Type,'update'),
    outerl:UpdateFuncName([State|lists:map(fun(X)->getAm(X) end, Depends)]).

calcInitState({Type,Depends},State) ->
    %lists:foreach(fun(Name) ->
    %                  initAm(Name)
    %              end, Depends),
    InitFuncName = dsler:calc_func_name(Type,'init'),
    io:format("InitFuncName = ~p (depends on ~p)\n", [InitFuncName,Depends]),
    outerl:InitFuncName([State|lists:map(fun(X)->getAm(X) end, Depends)]).

%set 1 automata, ignoring all dependencies
strictSingleSetAm(Name,State) ->
    [[Pid]] = ets:match(proctable,{Name,'$1'}),
    Pid ! {self(), {set, State}},
    receive
        {am,Any} -> Any
    after
        1000 -> error({timeout,strictSingleSetAm})
    end.

registerAm(Name, RegName) ->
    [[Pid]] = ets:match(proctable,{Name,'$1'}),
    Pid ! {self(), {register, RegName}},
    receive
        {am,Any} -> Any
    after
        1000 -> error({timeout,registerAm})
    end.

registerMe(Name, {_Type, Depends}) ->
    lists:foreach(fun(X) ->
                      registerAm(X, Name)
                  end, Depends).

updateAm(Name) ->
    [[Pid]] = ets:match(proctable,{Name,'$1'}),
    Pid ! {self(), {update}},
    receive
        {am,Any} -> Any
    after
        1000 -> erlang:error({timeout,updateAm})
    end.

initAm(Name) ->
    [[Pid]] = ets:match(proctable,{Name,'$1'}),
    Pid ! {self(), {init}},
    receive
        {am,Any} -> Any
    after
        1000 -> erlang:error({timeout,initAm})
    end.

getAm(Name) ->
    io:format("asking ~p\n",[Name]),
    [[Pid]] = ets:match(proctable,{Name,'$1'}),
    Pid ! {self(), {get}},
    receive
        {am,Any} -> Any
    after
        1000 -> erlang:error({timeout,getAm})
    end.

test() ->
    %initAm('A'),
    %initAm('B'),
    %initAm('S'),
    %timer:sleep(500),
    updateAm('B'),
    %getAm('A'),
    timer:sleep(20000),
    getAm('B').

main() ->
    InputFile = "file.txt",
    Grammar = grammar,
    OutFile = "outerl",
    {Classes, Ams} = dsler:parse_file(InputFile,Grammar),
    dsler:write_methods(Classes,OutFile),
    init(),
    lists:foreach(fun({Name,Type,_StateExpr}) -> addAm({Name,Type}) end,Ams),
    lists:foreach(fun({Name,_Type,_StateExpr}) -> initAm(Name) end,Ams),
    lists:foreach(fun({Name,Type,_StateExpr}) -> registerMe(Name,Type) end,Ams),
    timer:sleep(500),
    Ans = test(),
    Processes = ets:tab2list(proctable),
    lists:foreach(fun({_Name, Pid}) ->
                      exit(Pid,kill)
                  end, Processes),
    ets:delete(proctable),
    Ans.
