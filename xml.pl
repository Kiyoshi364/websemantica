:- use_module(library(sgml), [load_xml/3]).

:- use_module(library(lists), [foldl/4]).
:- use_module(library(reif), [if_/3, (=)/3]).

xml_clean(
  element(Name, Attrs, Cs0),
  element(Name, Attrs, Cs)
) :-
  if_(Cs0 = [[_|_]],
  Cs0 = Cs,
  foldl(skip_strings, Cs0, Cs, [])
  ),
true.

skip_strings([_|_], X, X).
skip_strings(element(N, A, C), [E | X], X) :-
  xml_clean(element(N, A, C), E).
