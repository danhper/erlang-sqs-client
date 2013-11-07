-ifndef(AWS_CONFIGURATION_HRL).
-define(AWS_CONFIGURATION_HRL, 1).

-record(aws_configuration, { endpoint :: string() }).
-type aws_configuration() :: #aws_configuration{}.

-endif.
