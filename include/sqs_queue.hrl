-ifndef(QUEUE_HRL).
-define(QUEUE_HRL, 1).

-include_lib("queue_attributes.hrl").

-record(sqs_queue, {
  name       :: string(),
  url        :: string(),
  attributes :: queue_attributes()
}).

-type sqs_queue() :: #sqs_queue{}.

-endif.
