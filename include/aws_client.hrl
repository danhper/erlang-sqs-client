-ifndef(AWS_CLIENT_HRL).
-define(AWS_CLIENT_HRL, 1).

-include_lib("aws_credentials.hrl").
-include_lib("aws_configuration.hrl").

-record(aws_client, { credentials :: aws_credentials(),
                      configuration :: aws_configuration(),
                      service :: string() }).
-type aws_client() :: #aws_client{}.

-endif.
