-module(message_attributes).

-include_lib("message_attributes.hrl").
-include_lib("http_wrapper.hrl").

-export([needed_attributes_params/1]).

-spec needed_attributes_params([string()]) -> [param()].
needed_attributes_params(Attributes) ->
  needed_attributes_params(Attributes, [], 1).

-spec needed_attributes_params([string()], [param()], integer()) -> param().
needed_attributes_params([], Params, _) -> Params;
needed_attributes_params([H|T], Params, N) ->
  Key = "AttributeName." ++ integer_to_list(N),
  Value = string_util:to_pascal_case(H),
  Param = http_wrapper:param(Key, Value),
  needed_attributes_params(T, [Param|Params], N + 1).


