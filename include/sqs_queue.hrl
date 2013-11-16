-ifndef(QUEUE_HRL).
-define(QUEUE_HRL, 1).

-include_lib("queue_attributes.hrl").

-record(queue, {
  name       :: string(),
  url        :: string(),
  attributes :: queue_attributes()
}).

-type queue() :: #queue{}.

-endif.
