-module(am).
-compile(export_all).

init() ->
    ets:new(proctable,[named_table]).

addAm({Name,Type}) ->
    Pid = spawn(?MODULE,fsmServer,[Name, Type,'$uninitialized',[]]),
    ets:insert(proctable,{Name,Pid}).

addAndSetAm({Name,Type,State}) ->
    addAm({Name,Type}),
    setSilentAm(Name,State).
    
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
    io:format("updateSpawn ~p\n",[Type]),
    Parent = self(),
    spawn(fun() ->
              Parent ! {self(), {setupdate, calcUpdateState(Type,State)}},
              receive
                  {am,Any} -> Any
              after
                  1000 -> error({timeout, thinkSpawn})
              end
          end).

notifySpawn(Dependents) ->
    spawn(fun() ->
              notifyDep(Dependents)
          end).

fsmServer(Name, Type, State,Dependents) ->
    io:format("created ~p (instance of ~p) = ~p\n",[self(),Type, State]),
    receive
        {From, {setupdate, NewState}} ->        
            From ! {am, ok},
            if
                (NewState /= State) ->
                    notifySpawn(Dependents); %notifyDep(Dependents);
                true ->
                    ok
            end,
            fsmServer(Name, Type, NewState, Dependents);
        {From, {setsilent, NewState}} ->        
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
            io:format("update received\n"),
            From ! {am, ok},
            updateSpawn(Type, State),
            fsmServer(Name, Type, State, Dependents);
        {From, {init}} ->           
            io:format("init received\n"),
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
setSilentAm(Name,State) ->
    [[Pid]] = ets:match(proctable,{Name,'$1'}),
    Pid ! {self(), {setsilent, State}},
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
    timer:sleep(5000),
    updateAm('A'),
    timer:sleep(5000),
    getAm('A').
    %getAm('B').

load({Name,_Type,nothing}) ->
    initAm(Name);
load({Name,_Type,StateExpr}) ->
    initAm(Name), setSilentAm(Name, dsler:eval_expr(StateExpr)).

main() ->
    InputFile = "file.txt",
    Grammar = grammar,
    OutFile = "outerl",
    {Classes, Ams} = dsler:parse_file(InputFile,Grammar),
    dsler:write_methods(Classes,OutFile),
    init(),
    lists:foreach(fun({Name,Type,_StateExpr}) -> addAm({Name,Type}) end,Ams),
    lists:foreach(fun(X) -> load(X) end, Ams),
    lists:foreach(fun({Name,Type,_StateExpr}) -> registerMe(Name,Type) end,Ams),
    timer:sleep(500),
    Ans = test(),
    Processes = ets:tab2list(proctable),
    lists:foreach(fun({_Name, Pid}) ->
                      exit(Pid,kill)
                  end, Processes),
    ets:delete(proctable),
    Ans.
