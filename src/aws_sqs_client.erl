-module(aws_sqs_client).
-include_lib("aws_sqs_client.hrl").
-include_lib("aws_credentials.hrl").
-include_lib("aws_configuration.hrl").
-export([create_client/3]).

-spec create_client(string(), string(), string()) -> aws_sqs_client().
create_client(AccessKey, SecretKey, Endpoint) ->
  Credentials = #aws_credentials{ access_key = AccessKey, secret_key = SecretKey },
  Configuration = #aws_configuration{ endpoint = Endpoint },
  #aws_sqs_client{ credentials = Credentials, configuration = Configuration }.
