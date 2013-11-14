-module(http_wrapper).

-include_lib("http_wrapper.hrl").
-export([
  execute_get/1,
  execute_get/2,
  execute_post/3,
  param/2,
  generate_header/1,
  canonical_header/1,
  signed_header/1,
  generate_param/1,
  generate_params/1,
  sort_by_key/1,
  append_slash/1
]).

-spec append_slash(string()) -> string().
append_slash(Url) ->
  case lists:last(Url) of
    $/ -> Url;
    _ -> Url ++ "/"
  end.

-spec generate_param(param()) -> string().
generate_param(Param) ->
  Param#param.key ++ "=" ++ http_uri:encode(Param#param.value).

-spec signed_header(param()) -> string().
signed_header(Header) ->
  string:to_lower(Header#param.key).

-spec canonical_header(param()) -> string().
canonical_header(Header) ->
  string:to_lower(Header#param.key) ++ ":" ++ string:strip(Header#param.value) ++ "\n".

-spec generate_header(param()) -> string().
generate_header(Header) ->
  Header#param.key ++ ":" ++ Header#param.value.


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
    % "text/xml" -> xmerl_scan:string(Content);
    _ -> Content
  end.

-spec handle_good_response(any()) -> response().
handle_good_response(Content) ->
  { { _, Status, _ }, Headers, Body } = Content,
  ContentTypeInfo = proplists:get_value("content-type", Headers),
  [ ContentType | _ ] = string:tokens(ContentTypeInfo, ";"),
  #response{ status = Status, content = parse_response(ContentType, Body) }.

-spec handle_response(any()) -> response().
handle_response(Response) ->
  { Flag, Content } = Response,
  case Flag of
    ok    -> handle_good_response(Content);
    error -> #response{ status = -1 }
  end.


-spec execute_get(request()) -> response().
execute_get(Request) ->
  BaseUrl = "http://" ++ Request#request.uri ++ Request#request.path,
  Url = generate_url(BaseUrl, Request#request.query),
  io:format("URL: ~s~n", [Url]),
  Headers = lists:map((fun(H) -> { H#param.key, H#param.value } end), Request#request.headers),
  Response = httpc:request(get, { Url, Headers}, [], []),
  handle_response(Response).

-spec execute_get(string(), [param()]) -> response().
execute_get(BaseUrl, Params) ->
  Url = generate_url(BaseUrl, Params),
  io:format("URL: ~s~n", [Url]),
  Response = httpc:request(get, { Url, []}, [], []),
  handle_response(Response).

-spec execute_post(string(), [param()], [param()]) -> response().
execute_post(BaseUrl, QueryParams, BodyParams) ->
  Url = generate_url(BaseUrl, QueryParams),
  BodyParamsString = generate_params(BodyParams),
  Response = httpc:request(post, { Url, [], "application/x-www-form-urlencoded", BodyParamsString}, [], []),
  handle_response(Response).


-spec param(string(), string()) -> param().
param(Key, Value) ->
  #param{ key = Key, value = Value }.

-spec key_sorter(param(), param()) -> boolean().
key_sorter(First, Second) ->
  First#param.key < Second#param.key.

-spec sort_by_key([param()]) -> [param()].
sort_by_key(Params) ->
  lists:sort(fun key_sorter/2, Params).
