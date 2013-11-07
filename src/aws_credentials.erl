-module(aws_credentials).

-include_lib("aws_client.hrl").

-export([generate_signature_key/1]).

-spec generate_signature_key(aws_client()) -> string().
generate_signature_key(Client) ->
  Key = Client#aws_client.credentials#aws_credentials.secret_key,
  DateStamp = binary_to_list(iso8601:format(now())),
  RegionName = "foo",
  ServiceName = "bar",
  generate_signature_key(Key, DateStamp, RegionName, ServiceName).


-spec generate_signature_key(string(), string(), string(), string()) -> string().
generate_signature_key(Key, DateStamp, RegionName, ServiceName) ->
  DateKey = hmac:hmac256("AWS4" ++ Key, DateStamp),
  RegionKey = hmac:hmac256(DateKey, RegionName),
  ServiceKey = hmac:hmac256(RegionKey, ServiceName),
  SigningKey = hmac:hmac256(ServiceKey, "aws4_request"),

  string:to_lower(hmac:hexlify(SigningKey)).
