-module(aws_sqs_client).

-include_lib("aws_sqs_client.hrl").
-include_lib("sqs_queue.hrl").
-include_lib("request_type.hrl").
-include_lib("http_wrapper.hrl").
-include_lib("queue_attributes.hrl").
-include_lib("sqs_message.hrl").
-include_lib("aws_response.hrl").

-import(http_wrapper, [param/2]).
-import(aws_credentials, [authenticate_get_request/2]).

-export([
  create_client/3,
  create_queue/2,
  create_queue/3,
  send_message/3,
  list_queues/1,
  list_queues/2
]).

-spec create_client(string(), string(), string()) -> aws_sqs_client().
create_client(AccessKey, SecretKey, Region) ->
  Credentials = #aws_credentials{ access_key = AccessKey, secret_key = SecretKey },
  Endpoint = "sqs." ++ Region ++ ".amazonaws.com",
  Configuration = #aws_configuration{ region = Region, endpoint = Endpoint },
  #aws_client{ credentials = Credentials,
               configuration = Configuration,
               service = "sqs" }.

-spec get_basic_info(aws_client(), request()) -> { string(), [param()], string() }.
get_basic_info(Client, Request) ->
  Host = Client#aws_client.configuration#aws_configuration.endpoint,
  VersionHeaders = case Request#request.method of
    "get"  -> [param("Version", ?API_VERSION)];
    "post" -> []
  end,
  Uri = case Request#request.uri of
    undefined  -> Host;
    RequestUri -> RequestUri
  end,
  { Host, VersionHeaders, Uri }.

-spec make_sqs_request(aws_client(), request()) -> response().
make_sqs_request(Client, BaseRequest) ->
  { Host, VersionHeaders, Uri } = get_basic_info(Client, BaseRequest),
  Request = BaseRequest#request{
    query   = BaseRequest#request.query ++ VersionHeaders,
    headers = BaseRequest#request.headers ++ [param("Host", Host)],
    uri     = Uri
  },
  AuthenticatedRequest = aws_credentials:authenticate_request(Client, Request),
  { Response, Status, { Content, _ } } = http_wrapper:execute_request(AuthenticatedRequest),
  case Response of
    response -> { Status, aws_response:parse_response(Content) };
    _        -> { Status, Content }
  end.

-spec create_queue(aws_sqs_client(), string()) -> response().
create_queue(Client, Name) ->
  create_queue(Client, Name, #queue_attributes{}).

-spec create_queue(aws_sqs_client(), string(), queue_attributes()) -> response().
create_queue(Client, Name, Attributes) ->
  Params = [
    param("Action", "CreateQueue"),
    param("QueueName", Name)
  ] ++ queue_attributes:to_params(Attributes),
  make_sqs_request(Client, #request{
    method="post",
    payload = http_wrapper:generate_params(Params)
  }).

-spec send_message(aws_sqs_client(), sqs_queue(), string()) -> response().
send_message(Client, Queue, Message) when is_list(Message) ->
  send_message(Client, Queue, #sqs_message{ content = Message });
send_message(Client, Queue, Message) ->
  Endpoint = Client#aws_client.configuration#aws_configuration.endpoint,
  Params = [param("Action", "SendMessage") | sqs_message:to_params(Message)],
  { Status, Response } = make_sqs_request(Client, #request{
    method  = "post",
    path    = sqs_queue:get_path(Queue, Endpoint),
    payload = http_wrapper:generate_params(Params)
  }),
  case Status of
    200 -> { Status, Response#aws_response.content#sqs_message{ content = Message }};
    _   -> { Status, Response }
  end.

-spec list_queues(aws_client()) -> response().
list_queues(Client) ->
  list_queues(Client, "").

-spec list_queues(aws_client(), string()) -> response().
list_queues(Client, Prefix) ->
  make_sqs_request(Client, #request{
    query = [param("Action", "ListQueues")] ++ if
      length(Prefix) > 0 -> [param("QueueNamePrefix", Prefix)];
      true -> []
    end
  }).
