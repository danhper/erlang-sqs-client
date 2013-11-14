-module(aws_credentials).

-include_lib("aws_credentials.hrl").
-include_lib("aws_client.hrl").
-include_lib("http_wrapper.hrl").

-import(http_wrapper, [
  param/2,
  generate_params/1,
  canonical_header/1,
  signed_header/1,
  sort_by_key/1,
  append_slash/1
]).

-export([
  authenticate_get_request/2,
  authenticate_post_request/2
]).

-spec hash(string()) -> string().
hash(Input) ->
  string:to_lower(hmac:hexlify(erlsha2:sha256(Input))).

-spec generate_canonical_request(request()) -> string().
generate_canonical_request(Request) ->
  Method = string:to_upper(Request#request.method),
  Path = http_wrapper:append_slash(Request#request.path),
  Query = http_wrapper:generate_params(Request#request.query),
  Headers = string:join(lists:map((fun(H) -> http_wrapper:canonical_header(H) end), Request#request.headers), ""),
  SignedHeaders = string:join(lists:map((fun(H) -> string:to_lower(H#param.key) end), Request#request.headers), ";"),
  HashedPayload = hash(Request#request.payload),
  string:join([Method, Path, Query, Headers, SignedHeaders, HashedPayload], "\n").

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

-spec generate_signature_key(string(), string(), string(), string()) -> string().
generate_signature_key(Key, DateStamp, RegionName, ServiceName) ->
  DateKey = hmac:hmac256("AWS4" ++ Key, DateStamp),
  RegionKey = hmac:hmac256(DateKey, RegionName),
  ServiceKey = hmac:hmac256(RegionKey, ServiceName),
  hmac:hmac256(ServiceKey, "aws4_request").

-spec get_base_info(aws_client(), request()) -> { calendar:datetime(), string(), string(), string(), string(), string() }.
get_base_info(Client, Request) ->
  Date = Request#request.date,
  DateStamp = date_util:to_iso8601_date(Date),
  Key = Client#aws_client.credentials#aws_credentials.access_key,
  Secret = Client#aws_client.credentials#aws_credentials.secret_key,
  RegionName = Client#aws_client.configuration#aws_configuration.region,
  ServiceName = Client#aws_client.service,
  { Date, DateStamp, Key, Secret, RegionName, ServiceName }.


-spec make_signature(request(), calendar:datetime(), string(), string(), string()) -> string().
make_signature(Request, Date, RegionName, ServiceName, Secret) ->
  HashedRequest = generate_hashed_request(Request),
  StringToSign = get_string_to_sign(Date, RegionName, ServiceName, HashedRequest),
  SigningKey = generate_signature_key(Secret, date_util:to_iso8601_date(Date), RegionName, ServiceName),
  generate_signature(SigningKey, StringToSign).

-spec get_signed_headers(request()) -> string().
get_signed_headers(Request) ->
  string:join(lists:map(fun http_wrapper:signed_header/1, Request#request.headers), ";").

-spec authenticate_get_request(aws_client(), request()) -> request().
authenticate_get_request(Client, BaseRequest) ->
  { Date, DateStamp, Key, Secret, RegionName, ServiceName } = get_base_info(Client, BaseRequest),
  AuthParams = [
     http_wrapper:param("X-Amz-Algorithm", ?ALGORITHM),
     http_wrapper:param("X-Amz-Credential", get_credentials(Key, DateStamp, RegionName, ServiceName)),
     http_wrapper:param("X-Amz-Date", date_util:to_iso8601(Date)),
     http_wrapper:param("X-Amz-SignedHeaders", get_signed_headers(BaseRequest))
  ],
  Params = http_wrapper:sort_by_key(BaseRequest#request.query ++ AuthParams),
  Request = BaseRequest#request{query = Params},
  Signature = make_signature(Request, Date, RegionName, ServiceName, Secret),
  FinalParams = Params ++ [http_wrapper:param("X-Amz-Signature", Signature)],
  Request#request{query = FinalParams}.

-spec authenticate_post_request(aws_client(), request()) -> request().
authenticate_post_request(Client, BaseRequest) ->
  { Date, DateStamp, Key, Secret, RegionName, ServiceName } = get_base_info(Client, BaseRequest),
  Headers = BaseRequest#request.headers ++ [
    http_wrapper:param("Content-type", "application/x-www-form-urlencoded; charset=utf-8"),
    http_wrapper:param("x-amz-date", date_util:to_iso8601(Date))
  ],
  Request = BaseRequest#request{headers = Headers},
  SignedHeaders = get_signed_headers(Request),
  Signature = make_signature(Request, Date, RegionName, ServiceName, Secret),
  AuthHeader = ?ALGORITHM ++ string:join([" Credential=" ++ get_credentials(Key, DateStamp, RegionName, ServiceName), "SignedHeaders=" ++ SignedHeaders, "Signature=" ++ Signature], ", "),
  FinalHeaders = Headers ++ [http_wrapper:param("Authorization", AuthHeader)],
  Request#request{headers = FinalHeaders}.
