-ifndef(AWS_SQS_CLIENT_HRL).
-define(AWS_SQS_CLIENT_HRL, 1).

-include_lib("aws_credentials.hrl").
-include_lib("aws_configuration.hrl").

-record(aws_sqs_client, { credentials :: aws_credentials,
                           configuration :: aws_configuration }).
-type aws_sqs_client() :: #aws_sqs_client{}.

-endif.
