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

-spec parse_response(string(), string()) -> any().
parse_response(Type, Content) ->
  case Type of
    "text/xml" -> xmerl_scan:string(Content);
    _ -> Content
  end.

-spec handle_response(any()) -> response()
handle_good_response(Content) ->
  { { _, Status, _ }, Headers, Body } = Content,
  ContentTypeInfo = proplists:get_value("content-type", Headers),
  [ ContentType | _ ] = string:tokens(ContentTypeInfo, ";"),
  #response{ status = Status, content = parse_response(ContentType, Body) }.

-spec handle_response(any()) -> response()
handle_response(Response) ->
  { Flag, Content } = Response,
  case Flag of
    ok    -> handle_good_response(Content);
    error -> #response{ status = -1 }
  end.

-spec execute_get(string(), [param()]) -> response().
execute_get(BaseUrl, Params) ->
  Url = generate_url(BaseUrl, Params),
  io:format("URL: ~s~n", [Url]),
  Response = httpc:request(get, { Url, []}, [], []),
  handle_response(Response).

-spec param(string(), string()) -> param().
param(Key, Value) ->
  #param{ key = Key, value = Value }.
