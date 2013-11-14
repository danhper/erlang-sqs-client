-module(aws_sqs_client).

-include_lib("aws_sqs_client.hrl").
-include_lib("request_type.hrl").
-include_lib("http_wrapper.hrl").
-include_lib("queue_attributes.hrl").

-import(http_wrapper, [param/2]).

-export([
  create_client/3,
  add_permission/1,
  create_queue/2,
  create_queue/3
]).

-spec create_client(string(), string(), string()) -> aws_sqs_client().
create_client(AccessKey, SecretKey, Region) ->
  Credentials = #aws_credentials{ access_key = AccessKey, secret_key = SecretKey },
  Endpoint = "http://sqs." ++ Region ++ ".amazonaws.com",
  Configuration = #aws_configuration{ region = Region, endpoint = Endpoint },
  #aws_client{ credentials = Credentials,
               configuration = Configuration,
               service = "sqs" }.

-spec execute_get_request(aws_sqs_client(), string(), [param()]) -> response().
execute_get_request(Client, UrlPath, BaseParams) ->
  BaseUrl = Client#aws_client.configuration#aws_configuration.endpoint,
  Url = BaseUrl ++ UrlPath,
  ParamsWithAuth = BaseParams ++ aws_credentials:generate_auth_params(Client),
  http_wrapper:execute_get(Url, ParamsWithAuth).


-spec add_permission(aws_sqs_client()) -> response().
add_permission(Client) ->
  execute_get_request(Client, "", []).

-spec create_queue(aws_sqs_client(), string()) -> response().
create_queue(Client, Name) ->
  create_queue(Client, Name, #queue_attributes{}).

-spec create_queue(aws_sqs_client(), string(), queue_attributes()) -> response().
create_queue(Client, Name, Attributes) ->
  Params = [
    param("Action", "CreateQueue"),
    param("QueueName", Name)
  ],
  execute_get_request(Client, "", Params).
