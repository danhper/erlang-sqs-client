-ifndef(HTTP_WRAPPER_HRL).
-define(HTTP_WRAPPER_HRL, 1).
-include_lib("xmerl/include/xmerl.hrl").

-type xml_element() :: #xmlElement{}.

-record(param, { key :: string(), value :: string() }).
-type param() :: #param{}.

-record(response, { status :: integer(), content :: any() }).
-type response() :: #response{}.

-endif.
