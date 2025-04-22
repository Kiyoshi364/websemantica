% :- module(turtle).

:- use_module(library(dcgs), []).
:- use_module(library(reif), [if_/3, (=)/3, memberd_t/3]).

:- use_module(reif_dcgs, [if_//3]).

matcheq(Elem, Cases) --> matcheq_impl(Cases, Elem, Cases).

matcheq_impl([], E, Cases) --> { throw(error(unreachable_match(E, Cases))) }.
matcheq_impl([If_2-Then | Cs], E, Cases) -->
  if_(call(If_2, E), Then, matcheq_impl(Cs, E, Cases)).

:- use_module(linecol, [char//2, unchar//2]).

:- use_module(library(debug)).

ws_t(C, T) :- memberd_t(C, " \t", T).
eof_t(C, T) :- =(eof, C, T).
comment_t(C, T) :- =('#', C, T).

comma_t(C, T) :- =(',', C, T).
semi_t(C, T) :- =(';', C, T).
dot_t(C, T) :- =('.', C, T).

comment(Comment) -->
  char(C, P),
  if_(C = '\n',
    ( { Comment = [] }, unchar(C, P) ),
    if_(C = eof,
      { Comment = [] },
      ( { Comment = [C|T] }, comment(T) )
    )
  ).

id(C0, C0) --> [].

token(T, P) -->
  char(C0, P0),
  matcheq(C0, [
    ws_t      - token(T, P),
    eof_t     - { T = eof, P = P0 },
    comment_t - ( { T = comment(C), P = P0 }, comment(C) ),
    comma_t   - { T = comma, P = P0 },
    semi_t    - { T = semi, P = P0 },
    dot_t     - { T = dot, P = P0 },
    =(C0 )    - ( { T = id(ID), P = P0 }, id(C0, ID) )
  ]).

simple_triple(Subj, Pred, Obj) -->
  node(sub, Subj), ws_, node(pred, Pred), ws_, node(obj, Obj).

node(Kind, A, S0, S) -->
  ( { S = S0 }, iri(A)
  ; { S = S0 }, literal(A)
  ; { S = S0, state_prefix(S, SP) }, prefixed(SP, A)
  ; { state_blank(S0, S, SB0, SB) }, blank_node(A, SB0, SB) % Kind != pred
  ; { S = S0 }, collection(A)
  ).
