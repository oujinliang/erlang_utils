-module(utils_inet).
-compile(export_all).

-type(ip4_address() ::  {0..255, 0..255, 0..255, 0..255}).

%% Get all ipv4 addresses
-spec get_addresses() -> [ip4_address()].
get_addresses() ->
    {ok, Addresses} = inet:getif(),
    [Addr || {Addr,_,_} <- Addresses].

%% Get all ipv4 addresses filtered by Filter
-spec get_addresses(fun()) -> [ip4_address()].
get_addresses(Filter) ->
    lists:filter(Filter, get_addresses()).

%% Get all site local addresses
-spec get_sitelocal_addrs() -> [ip4_address()].
get_sitelocal_addrs() ->
    get_addresses(fun is_sitelocal_addr/1).

-spec is_loopback_addr( ip4_address()) -> true | false.
is_loopback_addr({127,_,_,_}) -> true;
is_loopback_addr(_)           -> false.

%% Check if a ipv4 address is a site local address
-spec is_sitelocal_addr( ip4_address()) -> true | false.
is_sitelocal_addr({10,_,_,_})    -> true;
is_sitelocal_addr({172,16,_,_})  -> true;
is_sitelocal_addr({192,168,_,_}) -> true;
is_sitelocal_addr(_)             -> false.

%% format a ipv4 address to string
-spec format_address(ip4_address()) -> string().
format_address({A,B,C,D}) ->
    lists:flatten(io_lib:format("~b.~b.~b.~b", [A,B,C,D])).

parse_address(Addr) when is_list(Addr) -> 
    inet_parse:ipv4_address(Addr);
parse_address(Addr) when is_binary(Addr) -> 
    parse_address(binary_to_list(Addr)).

