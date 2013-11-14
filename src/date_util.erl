-module(date_util).

-export([
  to_iso8601/1,
  to_iso8601_date/1
]).

-type datetime() :: tuple(Date::calendar:date(),
                          Time::calendar:time()).
-type timestamp() :: tuple(MegaSecs::integer(),
                           Secs::integer(),
                           MicroSecs::integer()).

-spec to_iso8601(datetime() | timestamp()) -> string().
to_iso8601({_,_,_}=Timestamp) ->
  to_iso8601(calendar:now_to_datetime(Timestamp));
to_iso8601({{Y,Mo,D}, {H,Mn,S}}) ->
    FmtStr = "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    binary_to_list(list_to_binary(IsoStr)).

-spec to_iso8601_date(datetime() | timestamp()) -> string().
to_iso8601_date({_,_,_}=Timestamp) ->
  to_iso8601_date(calendar:now_to_datetime(Timestamp));
to_iso8601_date({{Y,Mo,D}, {_,_,_}}) ->
    FmtStr = "~4.10.0B~2.10.0B~2.10.0B",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D]),
    binary_to_list(list_to_binary(IsoStr)).
