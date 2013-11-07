-module(http_wrapper).
-include_lib("http_wrapper.hrl").
-export([execute_get/2, param/2]).

-spec append_slash(string()) -> string().
append_slash(Url) ->
  case lists:last(Url) of
    $/ -> Url;
    _ -> Url ++ "/"
  end.

-spec generate_param(param()) -> string().
generate_param(Param) ->
  Param#param.key ++ "=" ++ Param#param.value.

-spec generate_params([param()]) -> string().
generate_params(Params) ->
  StringParams = lists:map(fun generate_param/1, Params),
  string:join(StringParams, "&").

-spec generate_url(string(), [param()]) -> string().
generate_url(BaseUrl, Params) ->
  Url = append_slash(BaseUrl),
  case Params of
    []         -> Url;
    ParamsList -> Url ++ "?" ++ generate_params(ParamsList)
  end.

-spec execute_get(string(), [param()]) -> no_return().
execute_get(BaseUrl, Params) ->
  Url = generate_url(BaseUrl, Params),
  httpc:request(get, { Url, []}, [], []).

-spec param(string(), string()) -> param().
param(Key, Value) ->
  #param{ key = Key, value = Value }.
