-module(aws_error).

-include_lib("aws_error.hrl").
-include_lib("aws_response.hrl").

-export([
  parse_error_response/1,
  parse_error/2
]).

-import(xml_util, [get_text/1]).

-spec parse_error_response(xmlElement()) -> aws_response().
parse_error_response(Xml) ->
  lists:foldl(fun parse_error_node/2, #aws_response{}, Xml#xmlElement.content).

-spec parse_error_node(xmlElement(), aws_response()) -> aws_response().
parse_error_node(Xml, Response) ->
  case Xml#xmlElement.name of
    'Error' ->
      Error = lists:foldl(fun parse_error/2, #aws_error{}, Xml#xmlElement.content),
      Response#aws_response {content = Error };
    'RequestId' -> Response#aws_response { request_id = get_text(Xml) }
  end.

-spec parse_error(xmlElement(), aws_error()) -> aws_error().
parse_error(Xml, ErrorRecord) when is_record(Xml, xmlElement) ->
  case Xml#xmlElement.name of
    'Type'    -> ErrorRecord#aws_error{ type    = get_text(Xml) };
    'Code'    -> ErrorRecord#aws_error{ code    = get_text(Xml) };
    'Message' -> ErrorRecord#aws_error{ message = get_text(Xml) };
    'Detail'  -> ErrorRecord#aws_error{ detail  = get_text(Xml) };
    _         -> ErrorRecord
  end.
