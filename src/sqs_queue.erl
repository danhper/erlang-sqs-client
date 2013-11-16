-module(sqs_queue).

-include_lib("sqs_queue.hrl").
-include_lib("aws_response.hrl").

-export([parse_list_queues_result/1]).

-spec parse_list_queues_result(xmlElement()) -> [queue()].
parse_list_queues_result(Xml) ->
  lists:map(fun parse_queue_url/1, Xml#xmlElement.content).

-spec parse_queue_url(xmlElement()) -> queue().
parse_queue_url(Xml) ->
  #queue{ url = xml_util:get_text(Xml) }.
