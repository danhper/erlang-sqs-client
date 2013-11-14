-module(aws_credentials).

-include_lib("aws_credentials.hrl").
-include_lib("aws_client.hrl").
-include_lib("http_wrapper.hrl").

-import(http_wrapper, [
  param/2,
  generate_params/1,
  canonical_header/1
]).

-export([
  generate_signature_key/1,
  generate_signature_key/4,
  generate_auth_params/1,
  generate_canonical_request/1,
  generate_hashed_request/1,
  get_string_to_sign/4,
  generate_signature/2
]).

-spec hash(string()) -> string().
hash(Input) ->
  string:to_lower(hmac:hexlify(erlsha2:sha256(Input))).

-spec generate_canonical_request(request()) -> string().
generate_canonical_request(Request) ->
  Method = string:to_upper(Request#request.method),
  Uri = Request#request.uri,
  Query = http_wrapper:generate_params(Request#request.query),
  Headers = string:join(lists:map((fun(H) -> http_wrapper:canonical_header(H) end), Request#request.headers), ""),
  SignedHeaders = string:join(lists:map((fun(H) -> string:to_lower(H#param.key) end), Request#request.headers), ";"),
  HashedPayload = hash(Request#request.payload),
  string:join([Method, Uri, Query, Headers, SignedHeaders, HashedPayload], "\n").

-spec generate_signature(string(), string()) -> string().
generate_signature(SigningKey, StringToSign) ->
  string:to_lower(hmac:hexlify(hmac:hmac256(SigningKey, StringToSign))).

-spec generate_hashed_request(request()) -> string().
generate_hashed_request(Request) ->
  hash(generate_canonical_request(Request)).

-spec get_credentials(string(), string(), string(), string()) -> string().
get_credentials(Key, DateStamp, RegionName, ServiceName) ->
  string:join([Key, DateStamp, RegionName, ServiceName, "aws4_request"], "/").

-spec get_credential_scope(string(), string(), string()) -> string().
get_credential_scope(DateStamp, RegionName, ServiceName) ->
  string:join([DateStamp, RegionName, ServiceName, "aws4_request"], "/").

-spec get_string_to_sign(calendar:datetime(), string(), string(), string()) -> string().
get_string_to_sign(Date, RegionName, ServiceName, HashedRequest) ->
  Scope = get_credential_scope(date_util:to_iso8601_date(Date), RegionName, ServiceName),
  string:join([?ALGORITHM, date_util:to_iso8601(Date), Scope, HashedRequest], "\n").

-spec generate_auth_params(aws_client()) -> [param()].
generate_auth_params(Client) ->
    { Key, DateStamp, RegionName, ServiceName, Signature, Now } = generate_signature_key(Client),
    [http_wrapper:param("X-Amz-Algorithm", ?ALGORITHM),
     http_wrapper:param("X-Amz-Date", date_util:to_iso8601(Now)),
     http_wrapper:param("X-Amz-SignedHeaders", "content-type;host"),
     http_wrapper:param("X-Amz-Signature", Signature),
     http_wrapper:param("X-Amz-Credential", get_credentials(Key, DateStamp, RegionName, ServiceName))].

-spec generate_signature_key(aws_client()) -> string().
generate_signature_key(Client) ->
  AccessKey = Client#aws_client.credentials#aws_credentials.access_key,
  SecretKey = Client#aws_client.credentials#aws_credentials.secret_key,
  Now = now(),
  DateStamp = date_util:to_iso8601_date(Now),
  RegionName = Client#aws_client.configuration#aws_configuration.region,
  ServiceName = Client#aws_client.service,
  Signature = generate_signature_key(SecretKey, DateStamp, RegionName, ServiceName),
  { AccessKey, DateStamp, RegionName, ServiceName, Signature, Now }.


-spec generate_signature_key(string(), string(), string(), string()) -> string().
generate_signature_key(Key, DateStamp, RegionName, ServiceName) ->
  DateKey = hmac:hmac256("AWS4" ++ Key, DateStamp),
  RegionKey = hmac:hmac256(DateKey, RegionName),
  ServiceKey = hmac:hmac256(RegionKey, ServiceName),
  hmac:hmac256(ServiceKey, "aws4_request").

