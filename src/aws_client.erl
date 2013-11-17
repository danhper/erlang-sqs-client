-module(aws_client).

-include_lib("aws_client.hrl").

-export([endpoint/1]).

-spec endpoint(aws_client()) -> string().
endpoint(Client) ->
  Client#aws_client.configuration#aws_configuration.endpoint.
