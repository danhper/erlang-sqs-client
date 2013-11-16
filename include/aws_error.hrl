-ifndef(AWS_ERROR_HRL).
-define(AWS_ERROR_HRL, 1).

-include_lib("aws_response.hrl").

-record(aws_error, {
  type          :: string(),
  code          :: string(),
  message = ""  :: string(),
  detail  = ""  :: string()
}).

-type aws_error() :: #aws_error{}.

-endif.
