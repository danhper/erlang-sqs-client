-module(message_attributes).

-include_lib("message_attributes.hrl").
-include_lib("http_wrapper.hrl").
-include_lib("aws_response.hrl").

-export([
  needed_attributes_params/1,
  parse_attribute/2
]).

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

-spec parse_attribute(xmlElement(), message_attributes()) -> message_attributes().
parse_attribute(Xml, Attrs) ->
  Key = xml_util:get_text(hd(Xml)),
  Value = xml_util:get_text(lists:last(Xml)),
  case Key of
    "SenderId"      -> Attrs#message_attributes{ sender_id = Value };
    "SentTimestamp" -> Attrs#message_attributes{ sent_timestamp = list_to_integer(Value) };
    "ApproximateReceiveCount" -> Attrs#message_attributes{ approximate_receive_count = list_to_integer(Value) };
    "ApproximateFirstReceiveTimestamp" -> Attrs#message_attributes{ approximate_first_receive_timestamp = list_to_integer(Value) }
  end.
