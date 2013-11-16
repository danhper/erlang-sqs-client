-ifndef(AWS_RESPONSE_HRL).
-define(AWS_RESPONSE_HRL, 1).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("aws_error.hrl").
-include_lib("sqs_queue.hrl").

-type xmlElement() :: #xmlElement{}.

-type aws_result() :: aws_error() | [queue()].

-record(aws_response, {
  request_id  :: string(),
  content     :: aws_result(),
  type
}).

-type aws_response() :: #aws_response{}.

-endif.
