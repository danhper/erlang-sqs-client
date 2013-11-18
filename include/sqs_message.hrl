-ifndef(SQS_MESSAGE_HRL).
-define(SQS_MESSAGE_HRL, 1).

-record(sqs_message, {
  id               :: string(),
  message_id       :: string(),
  content          :: string(),
  md5sum           :: string(),
  delay            :: integer()
}).

-type sqs_message() :: #sqs_message{}.


-endif.
