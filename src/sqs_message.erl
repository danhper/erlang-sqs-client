-module(sqs_message).

-include_lib("sqs_message.hrl").
-include_lib("aws_response.hrl").

-import(xml_util, [get_text/1]).
-import(http_wrapper, [param/2]).

-export([
  parse_sqs_message/1,
  parse_sqs_messages_list/1,
  to_params/1,
  merge_messsages_list/2
]).

-spec parse_sqs_messages_list(xmlElement()) -> [sqs_message()].
parse_sqs_messages_list(Xml) ->
  lists:map(fun parse_sqs_message/1, Xml#xmlElement.content).

-spec parse_sqs_message(xmlElement()) -> sqs_message().
parse_sqs_message(Xml) ->
  lists:foldl(fun parse_sqs_message/2, #sqs_message{}, Xml#xmlElement.content).

-spec parse_sqs_message(xmlElement(), sqs_message()) -> sqs_message().
parse_sqs_message(Xml, Message) when is_record(Xml, xmlElement) ->
  case Xml#xmlElement.name of
    'MD5OfMessageBody' -> Message#sqs_message{ md5sum = get_text(Xml) };
    'MessageId'        -> Message#sqs_message{ message_id = get_text(Xml) };
    'Id'               -> Message#sqs_message{ id = get_text(Xml) };
    _                  -> Message
  end.

-spec to_params(sqs_message()) -> [param()].
to_params(Message) when is_record(Message, sqs_message) ->
  to_params(Message, "");
to_params(Messages) when is_list(Messages) ->
  list_to_params(Messages).

-spec to_params(sqs_message(), string()) -> [param()].
to_params(Message, Prefix) ->
  Params = [param(Prefix ++ "MessageBody", Message#sqs_message.content)],
  case Message#sqs_message.delay of
    undefined -> Params;
    Delay     -> [param(Prefix ++ "DelaySeconds", integer_to_list(Delay)) | Params]
  end.

-spec list_to_params([sqs_message()]) -> [param()].
list_to_params(Messages) -> list_to_params(Messages, [], 1).

-spec list_to_params([sqs_message()], [param()], integer()) -> [param()].
list_to_params([], Params, _) -> Params;
list_to_params([H|T], Params, N) ->
  list_to_params(T, param_for_list(H, N) ++ Params, N + 1).

-spec param_for_list(sqs_message(), integer()) -> [param()].
param_for_list(Message, N) ->
  Prefix = "SendMessageBatchRequestEntry." ++ integer_to_list(N) ++ ".",
  [param(Prefix ++ "Id", Message#sqs_message.id) |  to_params(Message, Prefix)].

-spec merge_messsages_list([sqs_message()], [sqs_message()]) -> [sqs_message()].
merge_messsages_list(BaseMessages, ResponseMessages) ->
  Dict = lists:foldl((fun(M, D) -> dict:store(M#sqs_message.id, M, D) end), dict:new(), BaseMessages),
  lists:map((fun(M) -> merge_messsages(Dict, M) end), ResponseMessages).

-spec merge_messsages(dict(), sqs_message()) -> sqs_message().
merge_messsages(MessagesDict, ResponseMessage) ->
  Message = dict:fetch(ResponseMessage#sqs_message.id, MessagesDict),
  ResponseMessage#sqs_message{ content = Message#sqs_message.content }.
