-module(string_util).

-export([to_snake_case/1]).

-spec to_snake_case(string()) -> string().
to_snake_case(Atom) when is_atom(Atom) ->
  list_to_atom(to_snake_case(atom_to_list(Atom)));
to_snake_case([]) -> [];
to_snake_case([H|T]) ->
  to_snake_case(T, [string:to_lower(H)]).

to_snake_case([], NewStr) -> NewStr;
to_snake_case([H|T], NewStr) when H >= $A, H =< $Z ->
  to_snake_case(T, NewStr ++ [$_, string:to_lower(H)]);
to_snake_case([H|T], NewStr) ->
  to_snake_case(T, NewStr ++ [H]).
