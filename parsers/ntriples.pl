% :- module(ntriples).

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

ws --> char(C), { memberd_t(C, " \t", true) }.
comma --> char(',').
semi --> char(';').
dot --> char('.').

comment -->
  char(C),
  if_(C = '#',
    ( skip_comment, { L = '\n' } ),
    { L = C }
  ),
  unchar(L).

skip_comment --> char(C), if_(C = '\n', [], skip_comment).

simple_triple(Subj, Pred, Obj) -->
  thing(Subj), ws_, thing(Pred), ws_, thing(Obj).
