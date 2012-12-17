-module(utils_erlang).


-export([to_binary/1,
         to_string/1,
         to_integer/1,
         to_term/1,
         trim/1, 
         trim_head/1, 
         trim_tail/1,
         is_whitespace/1,
         join_to_binary/2,
         concat_binary/1,
         for/2, for/3, for/4,
         any_child/1
        ]).

%% Convert to binary
-spec to_binary(string() | binary() | integer()) -> binary().

to_binary(Bin) when is_binary(Bin)  -> Bin;
to_binary(Int) when is_integer(Int) -> to_binary(integer_to_list(Int));
to_binary(Str) when is_list(Str)    -> list_to_binary(Str);
to_binary(Atom) when is_atom(Atom)  -> list_to_binary(atom_to_list(Atom));
to_binary(T)                        -> term_to_binary(T).

%% Convert to string
-spec to_string(integer() | string() | binary()) -> string().

to_string(Str) when is_list(Str)    -> Str;
to_string(Bin) when is_binary(Bin)  -> binary_to_list(Bin);
to_string(Int) when is_integer(Int) -> integer_to_list(Int);
to_string(Atom) when is_atom(Atom)  -> atom_to_list(Atom).

%% Convert to integer 
-spec to_integer(string() | integer() | binary()) -> integer().
to_integer(Int) when is_integer(Int) -> Int;
to_integer(Str) when is_list(Str)    -> list_to_integer(Str);
to_integer(Bin) when is_binary(Bin)  -> to_integer(binary_to_list(Bin)).

%% Convert a string/binary to erlang term.
-spec to_term(string() | binary()) -> term().
to_term(Input) when is_binary(Input) ->
    to_term(binary_to_list(Input));
to_term(Input) when is_list(Input) ->
    %%if input is not end with ".", add "." to it. 
    LastCharacter = string:right(Input, 1), 
    NormalizedInput =  case LastCharacter of
                            "." -> Input;
                            _ -> Input ++ "."
                       end,
    {ok,Tokens,_EndLine} = erl_scan:string(NormalizedInput),
    {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
    {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    {ok, Value}.

%% Trim the binary
-spec trim(Bin :: binary()) -> binary().
trim(Bin) when is_binary(Bin) -> trim_head(trim_tail(Bin)).

%% Trim head of binary
-spec trim_head(Bin :: binary()) -> binary().
trim_head(<<>>) -> <<>>;
trim_head(<<C, BinTail/binary>> = Bin) ->
    case is_whitespace(C) of
        true -> trim_head(BinTail);
        false -> Bin
    end.

%% Trim tail of binary
-spec trim_tail(Bin :: binary()) -> binary().
trim_tail(<<>>) -> <<>>;
trim_tail(Bin) ->
    Size = byte_size(Bin) - 1,
    <<BinHead:Size/binary,C>> = Bin,
    case is_whitespace(C) of
        true -> trim_tail(BinHead);
        false -> Bin
    end.

%% Check if the char is a whitespace
-spec is_whitespace(char()) -> true | false.
is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_)   -> false.

%% join multiple parts to binary, seperated by Sep.
-spec join_to_binary(Sep :: string() | binary() , Elements :: list()) -> binary().
join_to_binary(_Sep, []) -> <<>>;
join_to_binary(Sep, [Head|Tail]) ->
    concat_binary([Head | lists:append([[Sep, X] || X <- Tail]) ]).

%% Concat multiple parts to binary.
-spec concat_binary(List :: list()) -> binary().
concat_binary([]) -> <<>>;
concat_binary([_H | _T] = Elements) ->  
    << <<(to_binary(E))/binary>> || E <- Elements >>.

%% For loop.
for(N, Fun) when is_integer(N) ->
    for(0, N, 1, Fun).

%% For loop.
for(Start, End, Fun) when is_integer(Start), is_integer(End) ->
    for(Start, End, 1, Fun).

%% For loop
for(Start, Start, _Step, _Fun) when is_integer(Start)->
    ok;
for(Start, End, Step, Fun) when is_integer(Start), is_integer(End) ->
    Fun(Start), 
    for(Start + Step, End, Step, Fun).

%% Get any child of a supervisor.
any_child(Supervisor) ->
    Workers = supervisor:which_children(Supervisor),
    case length(Workers) of
        0 -> undefined;
        Len -> 
            {_Id, Pid, _Type, _Modules} = lists:nth(erlang:phash2(self(), Len), Workers),
            Pid
    end.

