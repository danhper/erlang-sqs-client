-module(aws_sqs_client).

-include_lib("aws_sqs_client.hrl").
-include_lib("request_type.hrl").
-include_lib("http_wrapper.hrl").

-import(queue_attributes, []).

-export([create_client/3
       , add_permission/1
]).

-spec create_client(string(), string(), string()) -> aws_sqs_client().
create_client(AccessKey, SecretKey, Region) ->
  Credentials = #aws_credentials{ access_key = AccessKey, secret_key = SecretKey },
  Endpoint = "http://sqs." ++ Region ++ ".amazonaws.com",
  Configuration = #aws_configuration{ region = Region, endpoint = Endpoint },
  #aws_client{ credentials = Credentials,
               configuration = Configuration,
               service = "sqs" }.

-spec execute_get_request(aws_sqs_client(), string(), integer(), [param()]) -> response().
execute_get_request(Client, UrlPath, Type, BaseParams) ->
  BaseUrl = Client#aws_client.configuration#aws_configuration.endpoint,
  Url = BaseUrl ++ UrlPath,
  ParamsWithAuth = BaseParams ++ aws_credentials:generate_auth_params(Client),
  http_wrapper:execute_get(Url, ParamsWithAuth).

-spec execute_request(aws_sqs_client(), string(), integer(), (post | get), [param()]) -> response().
execute_request(Client, UrlPath, Type, Method, Params) ->
  case Method of
    post ->
      execute_get_request(Client, UrlPath, Type, Params);
    get ->
      execute_get_request(Client, UrlPath, Type, Params)
  end.

add_permission(Client) ->
  execute_request(Client, "", ?ADD_PERMISSION, post, []).
