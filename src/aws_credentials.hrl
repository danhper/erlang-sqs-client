-ifndef(AWS_CREDENTIALS_HRL).
-define(AWS_CREDENTIALS_HRL, 1).

-record(aws_credentials, { access_key :: string(),
                           secret_key :: string() }).
-type aws_credentials() :: #aws_credentials{}.

-endif.
