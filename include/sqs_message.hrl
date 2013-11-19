-ifndef(SQS_MESSAGE_HRL).
-define(SQS_MESSAGE_HRL, 1).

-include_lib("message_attributes.hrl").

-record(sqs_message, {
  id               :: string(),
  message_id       :: string(),
  content          :: string(),
  md5sum           :: string(),
  delay            :: integer(),
  receipt_handle   :: string(),
  attributes = #message_attributes{} :: message_attributes()
}).

-type sqs_message() :: #sqs_message{}.

-record(receive_message_options, {
  max_number_of_messages = 1 :: integer(),
  visibility_timeout         :: integer(),
  wait_time_seconds          :: integer()
}).

-type receive_message_options() :: #receive_message_options{}.


-endif.
