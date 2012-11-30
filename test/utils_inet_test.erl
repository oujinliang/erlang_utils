-module(utils_inet_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

sample_test() ->
    ?assertEqual(true,  utils_inet:is_loopback_addr({127,0,0,1})),
    ?assertEqual(false, utils_inet:is_loopback_addr({137,0,0,1})),
    ?assertEqual(true,  utils_inet:is_sitelocal_addr({10,234,22,1})),
    ?assertEqual(true,  utils_inet:is_sitelocal_addr({10,0,22,1})),
    ?assertEqual(true,  utils_inet:is_sitelocal_addr({172,16,22,1})),
    ?assertEqual(true,  utils_inet:is_sitelocal_addr({192,168,22,1})),
    ?assertEqual(false, utils_inet:is_sitelocal_addr({129,168,22,1})),
    ok.

filter_loopback_test() ->
    ?assertEqual([{127,0,0,1}], utils_inet:get_addresses(fun utils_inet:is_loopback_addr/1)),
    ok.

format_address_test() ->
    Str = utils_inet:format_address({192,168,1,1}),
    ?assertEqual("192.168.1.1", Str),
    ok.
    
