-module(utils_time).

-export([now_ms/0, ms_to_now/1]).
-export([format_timestamp/2]).

-export([format_now_to_local_with_millisec/0, format_now_to_local_with_millisec/1,
         format_now_to_local/0, format_now_to_local/1]).

-define(SIMPLE_FORMAT_WITH_MILLISEC, "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~3..0w").
-define(SIMPLE_FORMAT_NO_MILLISEC, "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w").

%% Get current time in milliseconds.
-spec now_ms() -> non_neg_integer().
now_ms() ->
    timer:now_diff(os:timestamp(), {0,0,0}) div 1000.

%% Convert millisenconds to now format {MegaSecs, Secs, MicroSecs}
ms_to_now(Milliseconds) ->
    MicroSecs = (Milliseconds rem 1000) * 1000,
    Secs      = (Milliseconds div 1000) rem 1000000,
    MegaSecs  = (Milliseconds div 1000) div 1000000,
    {MegaSecs, Secs, MicroSecs}.

format_now_to_local() ->
    format_now_to_local(os:timestamp()).    
format_now_to_local({_MegaSecs, _Secs, _MicroSecs} = NowTime) ->
    format_timestamp(?SIMPLE_FORMAT_NO_MILLISEC, calendar:now_to_local_time(NowTime)).

format_now_to_local_with_millisec() ->
    format_now_to_local_with_millisec(os:timestamp()).
format_now_to_local_with_millisec({_MegaSecs, _Secs, MicroSecs} = NowTime) ->
    format_timestamp_with_millisec(?SIMPLE_FORMAT_WITH_MILLISEC, calendar:now_to_local_time(NowTime), MicroSecs div 1000).

format_timestamp(FormatStr, {{Year, Month, Day}, {Hour, Minute, Second}}) ->
    format_timestamp_impl(FormatStr, [Year, Month, Day, Hour, Minute, Second]).

format_timestamp_with_millisec(FormatStr, {{Year, Month, Day}, {Hour, Minute, Second}}, MilliSecs) ->
    format_timestamp_impl(FormatStr, [Year, Month, Day, Hour, Minute, Second, MilliSecs]).

format_timestamp_impl(FormatStr, Values) ->
    lists:flatten(io_lib:format(FormatStr, Values)).
    
