-module(aws_credentials).

-include_lib("aws_client.hrl").
-include_lib("http_wrapper.hrl").

-import(http_wrapper, [param/2]).

-export([
  generate_signature_key/1,
  generate_auth_params/1
]).

-spec get_credentials(string(), string(), string(), string()) -> string().
get_credentials(Key, DateStamp, RegionName, ServiceName) ->
  string:join([Key, DateStamp, RegionName, ServiceName, "aws4_request"], "/").

-spec generate_auth_params(aws_client()) -> [param()].
generate_auth_params(Client) ->
    { Key, DateStamp, RegionName, ServiceName, Signature, Now } = generate_signature_key(Client),
    [http_wrapper:param("X-Amz-Algorithm", "AWS4-HMAC-SHA256"),
     http_wrapper:param("X-Amz-Date", date_util:to_iso8601(Now)),
     http_wrapper:param("X-Amz-SignedHeaders", "content-type;host"),
     http_wrapper:param("X-Amz-Signature", Signature),
     http_wrapper:param("X-Amz-Credential", get_credentials(Key, DateStamp, RegionName, ServiceName))].

-spec generate_signature_key(aws_client()) -> string().
generate_signature_key(Client) ->
  Key = Client#aws_client.credentials#aws_credentials.secret_key,
  Now = now(),
  DateStamp = date_util:to_iso8601_date(Now),
  RegionName = Client#aws_client.configuration#aws_configuration.region,
  ServiceName = Client#aws_client.service,
  Signature = generate_signature_key(Key, DateStamp, RegionName, ServiceName),
  { Key, DateStamp, RegionName, ServiceName, Signature, Now }.


-spec generate_signature_key(string(), string(), string(), string()) -> string().
generate_signature_key(Key, DateStamp, RegionName, ServiceName) ->
  DateKey = hmac:hmac256("AWS4" ++ Key, DateStamp),
  RegionKey = hmac:hmac256(DateKey, RegionName),
  ServiceKey = hmac:hmac256(RegionKey, ServiceName),
  SigningKey = hmac:hmac256(ServiceKey, "aws4_request"),

  string:to_lower(hmac:hexlify(SigningKey)).

