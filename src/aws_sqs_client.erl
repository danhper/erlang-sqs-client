-module(aws_sqs_client).

-include_lib("aws_sqs_client.hrl").
-include_lib("request_type.hrl").
-include_lib("http_wrapper.hrl").

-export([create_client/3
       , add_permission/1
]).


-spec create_client(string(), string(), string()) -> aws_sqs_client().
create_client(AccessKey, SecretKey, Endpoint) ->
  Credentials = #aws_credentials{ access_key = AccessKey, secret_key = SecretKey },
  Configuration = #aws_configuration{ endpoint = Endpoint },
  #aws_sqs_client{ credentials = Credentials, configuration = Configuration }.

-spec execute_request(aws_sqs_client(), integer(), [param()]) -> response().
execute_request(Client, Type, Params) ->
  Url = Client#aws_sqs_client.configuration#aws_configuration.endpoint,
  Url.

add_permission(Client) ->
  execute_request(Client, ?ADD_PERMISSION, "foo").
