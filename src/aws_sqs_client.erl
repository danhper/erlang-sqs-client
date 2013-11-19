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
-import(aws_client, [endpoint/1]).

-export([
  create_client/3,
  create_queue/2,
  create_queue/3,
  send_message/3,
  send_message_batch/3,
  list_queues/1,
  list_queues/2,
  get_queue_url/2,
  delete_queue/2,
  receive_message/2,
  receive_message/3,
  receive_message/4
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

-spec make_sqs_request(aws_client(), request()) -> aws_response().
make_sqs_request(Client, BaseRequest) ->
  { Host, VersionHeaders, Uri } = get_basic_info(Client, BaseRequest),
  Request = BaseRequest#request{
    query   = BaseRequest#request.query ++ VersionHeaders,
    headers = BaseRequest#request.headers ++ [param("Host", Host)],
    uri     = Uri
  },
  AuthenticatedRequest = aws_credentials:authenticate_request(Client, Request),
  { _, _, { Content, _ } } = http_wrapper:execute_request(AuthenticatedRequest),
  aws_response:parse_response(Content).

-spec create_queue(aws_sqs_client(), string()) -> aws_response().
create_queue(Client, Name) ->
  create_queue(Client, Name, #queue_attributes{}).

-spec create_queue(aws_sqs_client(), string(), queue_attributes()) -> aws_response().
create_queue(Client, Name, Attributes) ->
  Params = [
    param("Action", "CreateQueue"),
    param("QueueName", Name)
  ] ++ queue_attributes:to_params(Attributes),
  make_sqs_request(Client, #request{
    method="post",
    payload = http_wrapper:generate_params(Params)
  }).

-spec send_message(aws_sqs_client(), sqs_queue(), string()) -> aws_response().
send_message(Client, Queue, Message) when is_list(Message) ->
  send_message(Client, Queue, #sqs_message{ content = Message });
send_message(Client, Queue, Message) ->
  Endpoint = Client#aws_client.configuration#aws_configuration.endpoint,
  Params = [param("Action", "SendMessage") | sqs_message:to_params(Message)],
  Response = make_sqs_request(Client, #request{
    method  = "post",
    path    = sqs_queue:get_path(Queue, Endpoint),
    payload = http_wrapper:generate_params(Params)
  }),
  case Response#aws_response.type of
    send_message_response ->
      Content = Response#aws_response.content,
      FixedContent = Content#sqs_message{ content = Message#sqs_message.content },
      Response#aws_response{ content = FixedContent };
    _   -> Response
  end.

-spec list_queues(aws_client()) -> aws_response().
list_queues(Client) ->
  list_queues(Client, "").

-spec list_queues(aws_client(), string()) -> aws_response().
list_queues(Client, Prefix) ->
  make_sqs_request(Client, #request{
    query = [param("Action", "ListQueues")] ++ if
      length(Prefix) > 0 -> [param("QueueNamePrefix", Prefix)];
      true -> []
    end
  }).

-spec delete_queue(aws_client(), sqs_queue()) -> aws_response().
delete_queue(Client, Queue) ->
  make_sqs_request(Client, #request{
    path = sqs_queue:get_path(Queue, endpoint(Client)),
    query = [param("Action", "DeleteQueue")]
  }).

-spec get_queue_url(aws_client(), sqs_queue() | string()) -> aws_response().
get_queue_url(Client, QueueName) when is_list(QueueName) ->
  get_queue_url(Client, #sqs_queue{ name = QueueName });
get_queue_url(Client, Queue) when is_record(Queue, sqs_queue) ->
  Params = [param("Action", "GetQueueUrl"),
            param("QueueName", Queue#sqs_queue.name)],
  make_sqs_request(Client, #request{
    query = case Queue#sqs_queue.owner_id of
      undefined -> Params;
      Owner     -> Params ++ [param("QueueOwnerAWSAccountId", Owner)]
    end
  }).

-spec send_message_batch(aws_client(), sqs_queue(), [sqs_message()]) -> aws_response().
send_message_batch(Client, Queue, Messages) ->
  Params = [param("Action", "SendMessageBatch") | sqs_message:to_params(Messages)],
  Response = make_sqs_request(Client, #request{
    method  = "post",
    path    = sqs_queue:get_path(Queue, endpoint(Client)),
    payload = http_wrapper:generate_params(Params)
  }),
  Content = Response#aws_response.content,
  UpdatedMessages = sqs_message:merge_messsages_list(Messages, Content),
  Response#aws_response{content = UpdatedMessages}.

-spec receive_message(aws_client(), sqs_queue()) -> aws_response().
receive_message(Client, Queue) -> receive_message(Client, Queue, #receive_message_options{}).

-spec receive_message(aws_client(), sqs_queue(), receive_message_options()) -> aws_response().
receive_message(Client, Queue, Options) -> receive_message(Client, Queue, Options, []).

-spec receive_message(aws_client(), sqs_queue(), receive_message_options(), [string()] | boolean()) -> aws_response().
receive_message(Client, Queue, Options, true) -> receive_message(Client, Queue, Options, ["all"]);
receive_message(Client, Queue, Options, false) -> receive_message(Client, Queue, Options, []);
receive_message(Client, Queue, Options, NeededAttrs) ->
  OptionsParams = sqs_message:receive_message_options_to_params(Options),
  AttrsParams = message_attributes:needed_attributes_params(NeededAttrs),
  Params = [param("Action", "ReceiveMessage")|OptionsParams ++ AttrsParams],
  make_sqs_request(Client, #request{
    path  = sqs_queue:get_path(Queue, endpoint(Client)),
    query = Params
  }).
