:- module(reif_dcgs, [
  if_//3
]).

:- use_module(library(dcgs), [phrase/3]).
:- use_module(library(reif), [if_/3, (=)/3]).

:- meta_predicate(if_(1, 2, 2, ?, ?)).

if_(If_1, Then_2, Else_2) -->
  { call(If_1, T) },
  ( { T == true  } -> phrase(Then_2)
  ; { T == false } -> phrase(Else_2)
  ; { nonvar(T) } -> { throw(error(type_error(boolean, T), _)) }
  ; { throw(error(instantiation_error, _)) }
  ).
