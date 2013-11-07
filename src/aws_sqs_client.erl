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
  Endpoint = "sqs." ++ Region ++ ".amazonaws.com",
  Configuration = #aws_configuration{ region = Region, endpoint = Endpoint },
  #aws_client{ credentials = Credentials,
               configuration = Configuration,
               service = "sqs" }.

-spec execute_request(aws_sqs_client(), integer(), [param()]) -> response().
execute_request(Client, Type, Params) ->
  Url = Client#aws_client.configuration#aws_configuration.endpoint,
  Url.

add_permission(Client) ->
  execute_request(Client, ?ADD_PERMISSION, "foo").
