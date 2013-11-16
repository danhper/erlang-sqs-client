-module(xml_util).

-include_lib("aws_response.hrl").

-export([get_text/1]).

-spec get_text(xmlElement() | [xmlElement()]) -> string().
get_text([]) -> "";
get_text([Node|_]) -> Node#xmlText.value;
get_text(SimpleNode) ->
  get_text(SimpleNode#xmlElement.content).
