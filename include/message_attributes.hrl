-ifndef(MESSAGE_ATTRIBUTES_HRL).
-define(MESSAGE_ATTRIBUTES_HRL, 1).

-record(message_attributes, {
  sender_id                           :: string(),
  sent_timestamp                      :: integer(),
  approximate_receive_count           :: integer(),
  approximate_first_receive_timestamp :: integer()
}).

-type message_attributes() :: #message_attributes{}.

-endif.
