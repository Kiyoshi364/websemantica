% :- module(turtle).

:- use_module(library(dcgs), []).
:- use_module(library(reif), [if_/3, (=)/3, memberd_t/3]).

matcheq(Elem, Cases) :- matcheq_t_impl(Cases, Elem).

matcheq_impl([], _).
matcheq_impl([C-Then | Cases], E) :-
  if_(E = C,
    ( call(Then), T = true ),
    matcheq_t_impl(Cases, E, T)
  ).

matcheq(Elem, Cases, S0, S) :- matcheq_t_impl(Cases, Elem, S0, S).

matcheq_impl([], _, false).
matcheq_impl([C-Then | Cases], E, T) :-
  if_(E = C,
    ( call(Then), T = true ),
    matcheq_t_impl(Cases, E, T)
  ).

:- use_module(reif_dcgs, [if_//3]).

:- use_module(linecol, [char//1, unchar//1]).

:- use_module(library(debug)).

ws_t(C, T) :- memberd_t(C, " \t", T).

comma --> char(',').
semi --> char(';').
dot --> char('.').

comment_t(C, T) :- =('#', C, T).
comment(Comment, P) -->
  char(C, P),
  if_(comment_t(C),
    ( skip_comment(Comment, PL), { L = '\n' } ),
    { L = C, PL = P }
  ),
  unchar(L, PL).

skip_comment(Comment, PL) -->
  char(C, P),
  if_(C = '\n',
    { Comment = [], PL = P },
    ( { Comment = [C|T] }, skip_comment(T, PL) )
  ).

simple_triple(Subj, Pred, Obj) -->
  node(sub, Subj), ws_, node(pred, Pred), ws_, node(obj, Obj).

node(Kind, A, S0, S) -->
  ( { S = S0 }, iri(A)
  ; { S = S0 }, literal(A)
  ; { S = S0, state_prefix(S, SP) }, prefixed(SP, A)
  ; { state_blank(S0, S, SB0, SB) }, blank_node(A, SB0, SB) % Kind != pred
  ; { S = S0 }, collection(A)
  ).
