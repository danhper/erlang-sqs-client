-module(aws_response).

-include_lib("aws_response.hrl").

-import(aws_error, [parse_error_response/1]).

-export([parse_response/1]).

-spec parse_request_metadata(xmlElement()) -> string().
parse_request_metadata(Xml) ->
  RequestId = hd(Xml#xmlElement.content),
  xml_util:get_text(RequestId).

-spec parse_response(xmlElement()) -> aws_response().
parse_response(Xml) ->
  case Xml#xmlElement.name of
    'ErrorResponse' -> aws_error:parse_error_response(Xml);
    _ -> parse_success_response(Xml)
  end.

-spec parse_success_response(xmlElement()) -> aws_response().
parse_success_response(Xml) ->
  lists:foldl(fun parse_success_node/2, #aws_response{}, Xml#xmlElement.content).

-spec parse_success_node(xmlElement(), aws_response()) -> aws_response().
parse_success_node(Xml, Response) ->
  case Xml#xmlElement.name of
    'ResponseMetadata' -> Response#aws_response { request_id = parse_request_metadata(Xml) };
    _ ->
      { Type, Content } = parse_response_content(Xml),
      Response#aws_response{ type = Type, content = Content }
  end.


-spec parse_response_content(xmlElement()) -> aws_result().
parse_response_content(Xml) ->
  case Xml#xmlElement.name of
    'ListQueuesResult' -> { list_queues_response, sqs_queue:parse_list_queues_result(Xml) }
  end.
