-module(queue_attributes).

-include_lib("queue_attributes.hrl").
-include_lib("http_wrapper.hrl").

-import(http_wrapper, [param/2]).

-export([to_params/1]).

-spec to_params(queue_attributes()) -> [param()].
to_params(Attrs) ->
  Params = [param("Attribute.1.Name", "VisibilityTimeout"),
            param("Attribute.1.Value", integer_to_list(Attrs#queue_attributes.visibility_timeout)),
            param("Attribute.2.Name", "DelaySeconds"),
            param("Attribute.2.Value", integer_to_list(Attrs#queue_attributes.delay_seconds)),
            param("Attribute.3.Name", "ReceiveMessageWaitTimeSeconds"),
            param("Attribute.3.Value", integer_to_list(Attrs#queue_attributes.receive_message_wait_time_seconds)),
            param("Attribute.4.Name", "MaximumMessageSize"),
            param("Attribute.4.Value", integer_to_list(Attrs#queue_attributes.maximum_message_size)),
            param("Attribute.5.Name", "MessageRetentionPeriod"),
            param("Attribute.5.Value", integer_to_list(Attrs#queue_attributes.message_retention_period))
            ],
  case Attrs#queue_attributes.policy of
    undefined -> Params;
    P -> Params ++ [
      param("Attribute.6.Name", "Policy"),
      param("Attribute.6.Value", P)
    ]
  end.
