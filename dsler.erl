-module(dsler).
-compile(export_all).

calc_func_name(ClassName,MethodName) ->
    list_to_atom(atom_to_list(ClassName) ++ ":" ++ atom_to_list(MethodName)).


reserved_word(Word) ->
    RW = erl_scan:reserved_word(Word),
    if
        RW -> true;
        true -> case Word of
            'begin' -> true;
            'class' -> true;
            'extends' -> true;
            _ -> false
        end
    end. 

parse_file(File,Grammar) ->
    yecc:file(Grammar),
    case compile:file(Grammar,[nowarn_format]) of %TODO:error reporting
        {ok,_} -> true;
        _ -> error('Grammar compilation error')
    end,
    code:purge(Grammar),
    code:load_file(Grammar),
    {ok, BinStr} = file:read_file(File),
    Str = "\n" ++ binary_to_list(BinStr),
    {ok,Lst,_N} = erl_scan:string(Str,0,{reserved_word_fun, (fun(X) -> reserved_word(X) end)}),
    {Ans, Res} = Grammar:parse(Lst ++ [{'$end','+infinity'}]),
    if 
        Ans == ok    -> Res;
        Ans == error -> 
            {Line_num, _Module, Message} = Res,
            io:format("Error parsing file '~s' with grammar '~s':\nIn line ~w: ",[File,Grammar,Line_num]),
            %io:format(Grammar:format_error(Message)),
            io:format([Message]),
            error("Parsing error")
    end.

validate_method_signature(Name,Args) ->
    case Name of
        'update' ->
             Args == [];
         _ ->
             true
    end.

%TODO:check if types of base classes have same dependencies
validate_extension(Dependencies,BaseClass) ->
    true.

print_method(ClassName,MethodName,MethodArgs,Method,Dependencies,ExtendsList) ->
    Valid = validate_method_signature(MethodName,MethodArgs),
    PrMethod = process_tree({ClassName,Dependencies,ExtendsList},Method),
    if
        Valid ->
            "\n'" ++ atom_to_list(calc_func_name(ClassName,MethodName)) ++ "'([This" ++ lists:map(fun(X)->","++atom_to_list(X) end,Dependencies) ++ "]" ++ lists:map(fun(X)->","++atom_to_list(X) end,MethodArgs) ++ ") ->\n    " ++ erl_pp:exprs(PrMethod) ++ ".\n";
        true ->
            error("Method "++atom_to_list(ClassName)++":"++atom_to_list(MethodName)++" seems to be not valid.")
    end.

write_methods(ClassList,OutFile) ->
    ListOfListsOfLists = lists:map(fun({class,ClassName,TypedDep,{extends,ExtendsList},Class}) -> lists:map(fun({member,MethodName,MethodArgs,Method}) -> print_method(ClassName,MethodName,MethodArgs,Method,lists:map(fun({A,_}) -> A end,TypedDep),ExtendsList) end,Class) end, ClassList),
    Module = "-module(" ++ OutFile ++ ").\n-compile(export_all).\n\n" ++ lists:append(lists:append(ListOfListsOfLists)),

    file:write_file(OutFile ++ ".erl",Module),
    case compile:file(OutFile) of
        {ok,_} -> true;
        _ -> error("Compilation error")
    end,
    code:purge(list_to_atom(OutFile)),
    code:load_file(list_to_atom(OutFile)).

process_tree(_Cl,{'var',B,C}) -> {'var',B,C};
process_tree(_Cl,{'integer',B,C}) -> {'integer',B,C};
process_tree(Cl,{A,B,Ch1}) -> {A,B,process_tree(Cl,Ch1)};
process_tree(Cl,{A,B,C,Ch1,Ch2}) -> {A,B,C,process_tree(Cl,Ch1),process_tree(Cl,Ch2)};

process_tree({ClassName,Dep,Ext},{'call',B,{'atom',C,Name},Args}) ->
    {'call',B,{'atom',C,calc_func_name(ClassName,Name)},element(2,erl_parse:parse_exprs([{'[',0}] ++ [{'var',0,'This'}] ++ lists:append(lists:map(fun(X) ->
                                                                                         [{',',0},{'var',0,X}]
                                                                                     end,
                                                                                     Dep))++[{']',0},{'dot',0}]))++Args};

%TODO:arguments from base class dependencies, not from this class
%TODO:if BaseClassName not found, interpret it as external module
process_tree({ClassName,Dep,Ext},{'call',B,{'remote',C,{'atom',D,BaseClassName},{'atom',E,Name}},Args}) ->
    {'call',B,{'atom',E,calc_func_name(BaseClassName,Name)},element(2,erl_parse:parse_exprs([{'[',0}] ++ [{'var',0,'This'}] ++ lists:append(lists:map(fun(X) ->
                                                                                         [{',',0},{'var',0,X}]
                                                                                     end,
                                                                                     Dep))++[{']',0},{'dot',0}]))++Args};

process_tree(Cl,List) -> lists:map(fun(X)->process_tree(Cl,X) end, List).
%TODO:process_tree(_Cl,Atom) -> Atom;





test(X) ->
    dsler:process_tree(element(2,erl_parse:parse_exprs(element(2,erl_scan:string(X))))).
parse(X) ->
    erl_parse:parse_exprs(element(2,erl_scan:string(X))).