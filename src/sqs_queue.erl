-module(sqs_queue).

-include_lib("sqs_queue.hrl").
-include_lib("aws_response.hrl").

-export([
  parse_list_queues_result/1,
  get_path/2
]).

-spec parse_list_queues_result(xmlElement()) -> [sqs_queue()].
parse_list_queues_result(Xml) ->
  lists:map(fun parse_queue_url/1, Xml#xmlElement.content).

-spec parse_queue_url(xmlElement()) -> sqs_queue().
parse_queue_url(Xml) ->
  #sqs_queue{ url = xml_util:get_text(Xml) }.

-spec get_path(string(), sqs_queue()) -> string().
get_path(Queue, Endpoint) ->
  PathStart = string:str(Queue#sqs_queue.url, Endpoint) + length(Endpoint),
  string:substr(Queue#sqs_queue.url, PathStart).
