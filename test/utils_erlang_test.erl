-module(utils_erlang_test).                                                                                                          
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-define(M, utils_erlang).

to_binary_test() ->
    ?assertEqual(<<"hello">>, ?M:to_binary("hello")),
    ?assertEqual(<<"123456">>, ?M:to_binary(123456)),
    ?assertEqual(<<"hello">>, ?M:to_binary(<<"hello">>)),
    ok.

to_string_test() ->
    ?assertEqual("56789", ?M:to_string(56789)),
    ?assertEqual("whatever", ?M:to_string("whatever")),
    ?assertEqual("A binary", ?M:to_string(<<"A binary">>)),
    ok.

to_integer_test() ->
    ?assertEqual(1234, ?M:to_integer("1234")),
    ?assertEqual(5678, ?M:to_integer(5678)),
    ?assertEqual(4567, ?M:to_integer(<<"4567">>)),
    ok.

to_term_test() ->
    {ok, ResultTerm} = ?M:to_term("{test, [123, \"string\"], <<\"binary\">> }"),
    ?assertEqual({test, [123, "string"], <<"binary">>}, ResultTerm),
    ok.

trim_test() ->
    ?assertEqual(<<"">>, ?M:trim(<<"">>)),
    ?assertEqual(<<"abc">>, ?M:trim(<<"abc">>)),
    ?assertEqual(<<"abc">>, ?M:trim(<<"  abc">>)),
    ?assertEqual(<<"abc">>, ?M:trim(<<"abc\t ">>)),
    ?assertEqual(<<"abc">>, ?M:trim(<<"\t\rabc\t ">>)),
                 
    ?assertEqual(<<"">>, ?M:trim_head(<<"">>)),
    ?assertEqual(<<"asf">>, ?M:trim_head(<<"asf">>)),
    ?assertEqual(<<"asf">>, ?M:trim_head(<<"   asf">>)),
    ?assertEqual(<<"asf  ">>, ?M:trim_head(<<"\r\n\tasf  ">>)),

    ?assertEqual(<<"">>, ?M:trim_tail(<<"">>)),
    ?assertEqual(<<"123">>, ?M:trim_tail(<<"123  ">>)),
    ?assertEqual(<<"123">>, ?M:trim_tail(<<"123 \r ">>)),
    ?assertEqual(<<" 123">>, ?M:trim_tail(<<" 123 \n ">>)),
    
    ok.

concat_binary_test() ->
    ?assertEqual(<<>>, ?M:concat_binary([])),
    ?assertEqual(<<>>, ?M:concat_binary([<<>>])),
    ?assertEqual(<<"true">>, ?M:concat_binary([true])),
    ?assertEqual(<<"ab,good123baby">>, ?M:concat_binary([a, b, <<",">>, "good", 123, "baby"])),
    ok.

join_to_binary_test() ->
    ?assertEqual(<<>>, ?M:join_to_binary(<<",">>, [])),
    Bin = ?M:join_to_binary(<<",">>, ["start", 123, "hello", <<"">>, "1234-90-90 12:90:33.xxx"]),
    ?assertEqual(<<"start,123,hello,,1234-90-90 12:90:33.xxx">>, Bin),
    ok.
