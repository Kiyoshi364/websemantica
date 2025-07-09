:- module(semweb_error, [
  must_be_triple/1,
  must_be_subject/1, must_be_predicate/1, must_be_object/1,
  triple_t/2,
  subject_t/2, predicate_t/2, object_t/2,
  iri_t/2, blank_t/2, literal_t/2,
  literal_obj_t/2,
  strlang_t/2, string_t/2, char_t/2,
  functor_t/4
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

must_be_predicate(P) :-
  ( var(P) -> throw(error(instantiation_error, must_be_predicate(P)))
  ; predicate_t(P, true) -> true
  ; throw(error(type_error(predicate, P), must_be_predicate/1))
  ).

must_be_object(O) :-
  ( var(O) -> throw(error(instantiation_error, must_be_object(O)))
  ; object_t(O, true) -> true
  ; throw(error(type_error(object, O), must_be_object/1))
  ).

triple_t(Triple, T) :- ','(functor_t(Triple, t, 3), triple_t_(Triple), T).
triple_t_(t(S, P, O), T) :- ','(subject_t(S), ( predicate_t(P), object_t(O) ), T).

subject_t(S, T) :- ;(iri_t(S), blank_t(S), T).
predicate_t(P, T) :-  iri_t(P, T).
object_t(O, T) :- ;(iri_t(O), ( blank_t(O) ; literal_t(O) ), T).

iri_t(Iri, T) :- functor_t(Iri, iri, 1, T).

blank_t(B, T) :- ','(functor_t(B, blank, 2), blank_t_(B), T).
blank_t_(blank(L, N), T) :- matcheq_t(L, [labeled-atom_t(N), unlabeled- =(true)], T).

literal_t(L, T) :- ','(functor_t(L, literal, 2), literal_t_(L), T).
literal_t_(literal(Ty, Str), T) :- ','(iri_t(Ty), literal_obj_t(Str), T).
literal_obj_t(X, T) :- if_(functor_t(X, @, 2), strlang_t_(X, T), string_t(X, T)).

strlang_t(SL, T) :- ','(functor_t(SL, @, 2), strlang_t_(SL), T).
strlang_t_(@(Str, Lang), T) :- ','(string_t(Str), string_t(Lang), T).

string_t(S, T) :-
  ( S == [] -> T = true
  ; if_(functor_t(S, '.', 2),
      ( S = [C | S1],
        ','(char_t(C), string_t(S1), T)
      ),
      ;(S = [], (S = [C | S1], char_t(C), string_t(S1)), T)
    )
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

atom_t(A, T) :-
  ( var(A) -> throw(error(instantiation_error, atom_t(A, T)))
  ; atom(A) -> T = true
  ; T = false
  ).

functor_t(F, N, A, T) :-
  ( nonvar(F) -> ( functor(F, N, A) -> T = true ; T = false )
  ; nonvar(N), nonvar(A) -> ( functor(F, N, A) -> T = true ; T = false )
  ; throw(error(instantiation_error, functor_t(F, N, A, T)))
  ).
