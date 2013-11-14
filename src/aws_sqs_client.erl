-module(aws_sqs_client).

-include_lib("aws_sqs_client.hrl").
-include_lib("request_type.hrl").
-include_lib("http_wrapper.hrl").
-include_lib("queue_attributes.hrl").

-import(http_wrapper, [param/2]).
-import(aws_credentials, [authenticate_get_request/2]).

-export([
  create_client/3,
  add_permission/1,
  create_queue/2,
  create_queue/3,
  send_message/2
]).

-spec create_client(string(), string(), string()) -> aws_sqs_client().
create_client(AccessKey, SecretKey, Region) ->
  Credentials = #aws_credentials{ access_key = AccessKey, secret_key = SecretKey },
  Endpoint = "sqs." ++ Region ++ ".amazonaws.com",
  Configuration = #aws_configuration{ region = Region, endpoint = Endpoint },
  #aws_client{ credentials = Credentials,
               configuration = Configuration,
               service = "sqs" }.


-spec execute_get_request(aws_sqs_client(), [param()]) -> response().
execute_get_request(Client, BaseParams) ->
  execute_get_request(Client, "/", BaseParams).

-spec execute_get_request(aws_sqs_client(), string(), [param()]) -> response().
execute_get_request(Client, UrlPath, BaseParams) ->
  Host = Client#aws_client.configuration#aws_configuration.endpoint,
  Version = http_wrapper:param("Version", "2012-11-05"),
  Request = #request{
    method  = "get",
    uri     = Host,
    path    = UrlPath,
    query   = BaseParams ++ [Version],
    headers = [http_wrapper:param("Host", Host)]
  },
  AuthenticatedRequest = aws_credentials:authenticate_get_request(Client, Request),
  http_wrapper:execute_get(AuthenticatedRequest).


-spec execute_post_request(aws_sqs_client(), string(), [param()], string()) -> response().
execute_post_request(Client, UrlPath, BaseParams, Body) ->
  Host = Client#aws_client.configuration#aws_configuration.endpoint,
  Version = http_wrapper:param("Version", "2012-11-05"),
  Request = #request{
    method  = "post",
    uri     = Host,
    path    = UrlPath,
    query   = BaseParams,
    headers = [http_wrapper:param("Host", Host)],
    payload = Body
  },
  AuthenticatedRequest = aws_credentials:authenticate_post_request(Client, Request),
  http_wrapper:execute_request(AuthenticatedRequest).

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
  execute_get_request(Client, Params).

-spec send_message(aws_sqs_client(), string()) -> response().
send_message(Client, Message) ->
  % Params = [
  %   param("Action", "SendMessage"),
  %   param("MessageBody", Message)
  % ],
  execute_post_request(Client, "/634433121014/foo", [], "Action=SendMessage&MessageBody=" ++ Message).
