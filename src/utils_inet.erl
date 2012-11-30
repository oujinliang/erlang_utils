-module(utils_inet).
-compile(export_all).

-type(ip4_address() ::  {0..255, 0..255, 0..255, 0..255}).

-spec get_addresses() -> [ip4_address()].
%% Get all ipv4 addresses
get_addresses() ->
    {ok, Addresses} = inet:getif(),
    [Addr || {Addr,_,_} <- Addresses].

get_addresses(Filter) ->
    lists:filter(Filter, get_addresses()).

get_sitelocal_addrs() ->
    get_addresses(fun is_sitelocal_addr/1).

-spec is_loopback_addr( ip4_address()) -> true | false.
is_loopback_addr({127,_,_,_}) -> true;
is_loopback_addr(_)           -> false.

%% Check if a ipv4 address is a site local address
-spec is_sitelocal_addr( ip4_address()) -> true | false.
is_sitelocal_addr({10,_,_,_})    -> true;
is_sitelocal_addr({172,B,_,_})   -> B >= 16 andalso B =< 31;
is_sitelocal_addr({192,168,_,_}) -> true;
is_sitelocal_addr(_)             -> false.

format_address({A,B,C,D}) ->
    lists:flatten(io_lib:format("~b.~b.~b.~b", [A,B,C,D])).

