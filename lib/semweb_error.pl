:- module(semweb_error, [
  must_be_triple/1,
  must_be_subject/1, must_be_verb/1, must_be_object/1,
  triple_t/2,
  subject_t/2, verb_t/2, object_t/2,
  resource_t/2, resource_iri_t/2, literal_t/2,
  blank_t/3, iri_t/2,
  strlang_t/2, string_t/2, char_t/2
]).

:- use_module(library(reif), [
  if_/3, (=)/3, (',')/3, (;)/3
]).

must_be_triple(T) :-
  ( var(T) -> throw(error(instantiation_error, must_be_triple(T)))
  ; triple_t(T, true) -> true
  ; throw(error(type_error(triple, T), must_be_triple/1))
  ).

must_be_subject(S) :-
  ( var(S) -> throw(error(instantiation_error, must_be_subject(S)))
  ; subject_t(S, true) -> true
  ; throw(error(type_error(subject, S), must_be_subject/1))
  ).

must_be_verb(V) :-
  ( var(V) -> throw(error(instantiation_error, must_be_verb(V)))
  ; verb_t(V, true) -> true
  ; throw(error(type_error(verb, V), must_be_verb/1))
  ).

must_be_object(O) :-
  ( var(O) -> throw(error(instantiation_error, must_be_object(O)))
  ; object_t(O, true) -> true
  ; throw(error(type_error(object, O), must_be_object/1))
  ).

triple_t(Triple, T) :- ','(Triple = t(S, V, O), ( subject_t(S), verb_t(V), object_t(O) ), T).

subject_t(S, T) :- resource_t(S, T).
verb_t(V, T) :-  resource_iri_t(V, T).
object_t(O, T) :- ;(resource_t(O), literal_t(O), T).

resource_t(R, T) :-
  ','(
    R = resource(Ty, N),
    matcheq_t(Ty, [
      iri       - iri_t(N),
      label(L)  - blank_t(L, N)
    ]),
    T
  ).
resource_iri_t(R, T) :- ','(R = resource(iri, N), iri_t(N), T).
literal_t(L, T) :- ','(L = literal(Ty, Str), ( resource_iri_t(Ty), literal_obj_t(Str) ), T).
literal_obj_t(X, T) :- matcheq_t(X, [@(_, _)-strlang_t(X), X-string_t(X)], T).

blank_t(L, N, T) :- matcheq_t(L, [labeled-iri_t(N), unlabeled- =(true)], T).

strlang_t(L, T) :- ','(L = @(Str, Lang), (string_t(Str), string_t(Lang)), T).

iri_t(Iri, T) :-
  ( var(Iri) -> throw(error(instantiation_error, iri_t(Iri, T)))
  ; atom(Iri) -> T = true
  ; T = false
  ).

string_t(S, T) :-
  ( S == [] -> T = true
  ; nonvar(S), functor(S, '.', 2) ->
    S = [C | S1],
    ','(char_t(C), string_t(S1), T)
  ; ;(S = [], (S = [C | S1], char_t(C), string_t(S1)), T)
  ).

char_t(C, T) :-
  ( var(C) -> throw(error(instantiation_error, char_t(C, T)))
  ; atom(C), atom_length(C, 1) -> T = true
  ; T = false
  ).

matcheq_t(E, Cs, T) :- matcheq_t_(Cs, E, T).

matcheq_t_([], _, false).
matcheq_t_([X-P_1 | Cs], E, T) :-
  if_(E = X, call(P_1, T), matcheq_t_(Cs, E, T)).
