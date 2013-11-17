-module(aws_response).

-include_lib("aws_response.hrl").

-import(aws_error, [parse_error_response/1]).

-export([
  parse_response/1,
  content/1
]).

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
  Type = string_util:to_snake_case(Xml#xmlElement.name),
  Response = #aws_response{ type = Type },
  lists:foldl(fun parse_success_node/2, Response, Xml#xmlElement.content).

-spec parse_success_node(xmlElement(), aws_response()) -> aws_response().
parse_success_node(Xml, Response) ->
  case Xml#xmlElement.name of
    'ResponseMetadata' -> Response#aws_response { request_id = parse_request_metadata(Xml) };
    _ ->
      Content = parse_response_content(Xml),
      Response#aws_response{ content = Content }
  end.


-spec parse_response_content(xmlElement()) -> aws_result().
parse_response_content(Xml) ->
  case Xml#xmlElement.name of
    'ListQueuesResult'  -> sqs_queue:parse_list_queues_result(Xml);
    'SendMessageResult' -> sqs_message:parse_sqs_message(Xml);
    'CreateQueueResult' -> sqs_queue:parse_single_queue_result(Xml);
    'GetQueueUrlResult' -> sqs_queue:parse_single_queue_result(Xml);
    _                   -> Xml
  end.

-spec content(aws_response()) -> aws_result().
content(Response) ->
  Response#aws_response.content.
