# erlang-sqs-client

Erlang client for Amazon SQS

## Development

### Dependencies
* rebar
* make

### Build
To build the sqs_client library just run make.

```
$ make
```

## Usage


### Installation

The simplest way to install the library is to use rebar.

Simply add

```erlang
{sqs_client, ".*", {git, "/path/to/repository"}}
```

or if you do not have a repository, copy the directory as `sqs_client` in your project directory and add

```erlang
{sub_dirs, ["sqs_client"]}.
```

to your `rebar.config` file.


### Sample usage

Assuming that the library is in the subdirectory `sqs_client` of your project, here is a minimal sample of the library usage.

```erlang
-module(foo).

-export([sqs_example/0]).

-include_lib("sqs_client/include/aws_sqs_client.hrl").
-include_lib("sqs_client/include/queue_attributes.hrl").
-include_lib("sqs_client/include/sqs_message.hrl").
-import(aws_response, [content/1]).

sqs_example() ->
  Client = aws_sqs_client:create_client("AWS_KEY", "AWS_SECRET", ?TOKYO),

  % create queue
  QueuesAttribute = #queue_attributes{ delay_seconds = 10 },
  Queue = content(aws_sqs_client:create_queue(Client, "queue_name", QueuesAttribute)),

  % send message to queue
  MessageResponse = content(aws_sqs_client:send_message(Client, Queue, "My message")),

  % MessageResponse#aws_response.type should be send_message_response


  % get queues with prefix "q", ignore second parameters for all queues
  Queues = content(aws_sqs_client:list_queues(Client, "q")),
  FirstQueue = hd(Queues),

  % receive messages from queue
  % set wait_time_seconds to enable long polling
  % set max_number_of_messages to an integer between 1 and 10 to set the number of messages to receive
  ReceiveOptions = #receive_message_options{ wait_time_seconds = 10, max_number_of_messages = 5 },
  Messages = aws_sqs_client:receive_message(Client, FirstQueue, ReceiveOptions),


  FirstMessage = hd(Messages),

  % process FirstMessage...

  % delete message
  aws_sqs_client:delete_message(Client, FirstMessage).
```


## Public API

### General design

The library is designed to respect as much as possible the SQS API, so most of the function, records and properties names are the one described in the official documentation. However, the case respect Erlang conventions (ie. `CreateQueue` action will be `create_queue` function).

### General remarks

All API calls functions take a `aws_sqs_client()` as their first argument, and return an `aws_response`. The `aws_response` record is defined as bellow.

```erlang
-record(aws_response, {
  request_id  :: string(),
  content     :: aws_result(),
  type
}).
```

and the content can be extracted using the `aws_response:content` function.


### Public API

#### `create_client`

```erlang
create_client(string(), string(), string()) -> aws_sqs_client().
```

The `create_client` function takes the AWS Access Key, AWS Secret Key, and the AWS region, and returns and `aws_sqs_client` record that will be needed for all other API calls.

Some macros for the regions are available in `aws_sqs_client.hrl`. The available regions are

* VIRGINIA
* CALIFORNIA
* OREGON
* IRELAND
* SINGAPORE
* SYDNEY
* TOKYO
* SAOPOLO

#### `create_queue`

```erlang
create_queue(aws_sqs_client(), string()) -> aws_response().
create_queue(aws_sqs_client(), string(), queue_attributes()) -> aws_response().
```

The `create_queue` function takes the `aws_sqs_client` and the name of the queue to create. Optionally, it can also take a `queue_attributes` record, making it possible to change the queue options [as defined in the documentation](http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/Query_QueryCreateQueue.html).

The content of the response is a `sqs_queue` record with a valid URL.

#### `list_queues`

```erlang
list_queues(aws_client()) -> aws_response().
list_queues(aws_client(), string()) -> aws_response().
```

The `list_queues` function takes the `aws_sqs_client` and optionally the wanted prefix for the queues to list.

The content of the response is a list of `sqs_queue` records.

#### `get_queue_url`

```erlang
get_queue_url(aws_client(), sqs_queue() | string()) -> aws_response().
```

The `get_queue_url` function takes the `aws_sqs_client` and a `sqs_queue` record with a valid name, or simply a `string` with the name of the queue to look for.

The content of the response is the `sqs_queue` with a valid URL.

#### `delete_queue`

```erlang
delete_queue(aws_client(), sqs_queue()) -> aws_response().
```

The `delete_queue` function takes the `aws_sqs_client` and the `sqs_queue` to delete. The response does not have any content.


#### `send_message`

```erlang
send_message(aws_sqs_client(), sqs_queue(), sqs_message() | string()) -> aws_response().
```

The `send_message` function takes the `aws_sqs_client`, the `sqs_queue` to post the message to and the message to be sent. The message to be sent can either be a `string` containing the body of the message, or a `sqs_message` with a valid `body`. When the third parameter is an `sqs_message`, the `delay_seconds` parameter can be set on the message.

The response will be a `sqs_message` with its `body`, `md5sum` and `message_id` filled in.


#### `send_message_batch`

```erlang
send_message_batch(aws_client(), sqs_queue(), [sqs_message()]) -> aws_response().
```

The `send_message_batch` function takes the `aws_sqs_client`, the `sqs_queue` to post the messages to, and a list of `sqs_message`. The `sqs_message` must all have a different `id` set for the request to work.

The content of the response will be a list of `sqs_message` with their `id`, `body`, `md5sum` and `message_id` filled in.

#### `receive_message`

```erlang
receive_message(aws_client(), sqs_queue()) -> aws_response().
receive_message(aws_client(), sqs_queue(), receive_message_options()) -> aws_response().
receive_message(aws_client(), sqs_queue(), receive_message_options(), [string()] | boolean()) -> aws_response().
```

The `receive_message` function takes the `aws_sqs_client` and the `sqs_queue` to fetch messages from. It can optionally takes a `receive_messages_options` as a third parameter, in order to set the `maximum_number_of_messages`, the `visiblility_timeout` and the `wait_time_seconds`. If no parameters is given, the default values will be used. It can also have a forth parameter to indicate the wanted attributes. The parameter can either be `true` for all the values, or a list of `string` with the names of the wanted attributes. The attributes names can be either snaked_cased or PascalCased.

The content of the response will be a list of `sqs_message` (even if only one message returned) with all the required fields and the requested attributes filled in.

#### `delete_message`

```erlang
delete_message(aws_client(), sqs_message()) -> aws_response().
```

The `delete_message` functino takes the `aws_sqs_client` and the `sqs_message` to delete. The `sqs_message` must have a valid `receipt_handle` for the API call to work properly.

The response does not contain any content.
