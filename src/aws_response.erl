-module(aws_response).

-include_lib("aws_response.hrl").

-import(aws_error, [parse_error_response/1]).

-export([parse_response/1]).

-spec parse_response(xmlElement()) -> aws_response().
parse_response(Xml) ->
  case Xml#xmlElement.name of
    'ErrorResponse' -> aws_error:parse_error_response(Xml)
  end.
