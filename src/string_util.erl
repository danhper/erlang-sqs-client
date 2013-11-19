-module(string_util).

-export([
  to_snake_case/1,
  to_camel_case/1,
  to_pascal_case/1
]).

-spec to_snake_case(string()) -> string().
to_snake_case(Atom) when is_atom(Atom) ->
  list_to_atom(to_snake_case(atom_to_list(Atom)));
to_snake_case([]) -> [];
to_snake_case([H|T]) ->
  to_snake_case(T, [string:to_lower(H)]).

-spec to_snake_case(string(), string()) -> string().
to_snake_case([], NewStr) -> NewStr;
to_snake_case([H|T], NewStr) when H >= $A, H =< $Z ->
  to_snake_case(T, NewStr ++ [$_, string:to_lower(H)]);
to_snake_case([H|T], NewStr) ->
  to_snake_case(T, NewStr ++ [H]).

-spec to_camel_case(string()) -> string().
to_camel_case(Str) -> to_camel_case(Str, [], false).

-spec to_pascal_case(string()) -> string().
to_pascal_case(Str) -> to_camel_case(Str, [], true).

-spec to_camel_case(string(), string(), boolean()) -> string().
to_camel_case([], CamelCaseStr, _) -> CamelCaseStr;
to_camel_case([H|T], CamelCaseStr, _) when H == $_ ->
  to_camel_case(T, CamelCaseStr, true);
to_camel_case([H|T], CamelCaseStr, true) ->
  to_camel_case(T, CamelCaseStr ++ [string:to_upper(H)], false);
to_camel_case([H|T], CamelCaseStr, false) ->
  to_camel_case(T, CamelCaseStr ++ [H], false).
