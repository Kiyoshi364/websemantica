:- module(semweb_prefix, [
  empty_prefixes/1, is_prefixes/1,
  list_to_prefixes/2, prefixes_to_list/2,
  put_prefixes/4,
  del_prefixes/4,
  get_prefixes/3,
  strip_prefix/3, unstrip_prefix/3
]).

:- use_module(library(lists), [foldl/4]).
:- use_module(library(reif), [dif/3, tfilter/3, tmember/2]).

empty_prefixes([]).

is_prefixes([P-N | PP]) :- atom(P), atom(N), is_prefixes(PP).

list_to_prefixes(L, PP) :-
  empty_prefixes(PP0),
  foldl(put_prefixes, L, PP0, PP).

prefixes_to_list(PP, PP).

put_prefixes(P, N, PP0, [P-N | PP]) :-
  tfilter(key_dif(P), PP0, PP).

del_prefixes(P, PP, PP0) :- tfilter(key_dif(P), PP, PP0).
del_prefixes(P, N, PP, PP0) :- tfilter(key_dif(P, N), PP, PP0).

get_prefixes(P, N, PP) :-
  tmember(=(P-N), PP).

key_dif(K, K1-_, T) :- dif(K, K1, T).
key_dif(K, V, K1-V, T) :- dif(K, K1, T).

strip_prefix(PP, R, R0) :-
  ( var(R) -> true
  ; strip_prefix_(R, PP, R0)
  ).

strip_prefix_(resource(Type, X), PP, R0) :- strip_prefix_resource(Type, X, PP, R0).
strip_prefix_(literal(Type, Str), PP, literal(Type0, Str)) :- strip_prefix(PP, Type, Type0).

strip_prefix_resource(iri, X, _, resource(iri, X)).
strip_prefix_resource(label(L), X, _, resource(label(L), X)).
strip_prefix_resource(prefixed, P:L, PP, resource(iri, R0)) :-
  get_prefixes(P, N, PP),
  atom_concat(N, L, R0).

unstrip_prefix(PP, R, R0) :-
  ( nonvar(R) -> true
  ; unstrip_prefix_(R0, R, PP)
  ).

unstrip_prefix_(resource(Type, X), R, PP) :- unstrip_prefix_resource(Type, X, R, PP).

unstrip_prefix_resource(iri, X, R, PP) :-
  % TODO
  tmember(is_prefix(X, P), PP).
unstrip_prefix_resource(label(L), X, R, PP) :-
  % TODO
  true.
unstrip_prefix_resource(prefixed, X, resource(prefixed, X), _).
