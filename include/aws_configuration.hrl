-ifndef(AWS_CONFIGURATION_HRL).
-define(AWS_CONFIGURATION_HRL, 1).

-record(aws_configuration, { region:: string(), endpoint :: string() }).
-type aws_configuration() :: #aws_configuration{}.

-endif.
