-ifndef(AWS_CLIENT_HRL).
-define(AWS_CLIENT_HRL, 1).

-include_lib("aws_credentials.hrl").
-include_lib("aws_configuration.hrl").

-record(aws_client, { credentials :: aws_credentials(),
                      configuration :: aws_configuration(),
                      service :: string() }).
-type aws_client() :: #aws_client{}.

-define(VIRGINIA,   "us-east-1").
-define(CALIFORNIA, "us-west-1").
-define(OREGON,     "us-west-2").
-define(IRELAND,    "eu-west-1").
-define(SINGAPORE,  "ap-southeast-1").
-define(SYDNEY,     "ap-southeast-2").
-define(TOKYO,      "ap-northeast-1").
-define(SAOPOLO,    "sa-east-1").

-endif.
