% :- module(turtle).

:- use_module(library(dcgs), []).
:- use_module(library(reif), [(=)/3, memberd_t/3]).

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

id(C0, L0, tkn(L0, id(C0))) --> [].

token(T) -->
  char(C0, L0),
  matcheq(C0, [
    ws_t      - token(T),
    eof_t     - { T = eof },
    comment_t - ( { T = tkn(L0, comment(C)) }, comment(C) ),
    comma_t   - { T = tkn(L0, comma) },
    semi_t    - { T = tkn(L0, semi) },
    dot_t     - { T = tkn(L0, dot) },
    =('[')    - { T = tkn(L0, open_square) },
    =(']')    - { T = tkn(L0, close_square) },
    =(C0 )    - id(C0, L0, T)
  ]).

if_token(tkn(_, Inner), Match_2, Then_2, Else_2) -->
  if_(call(Match_2, Inner), Then_2, Else_2).

matcheq_expect_token(tkn(Loc, Inner), PredName, Cases) --> matcheq_expect_token_impl(Cases, Inner, Loc, PredName, X, X).

matcheq_expect_token_impl([], Inner, Loc, PredName, Expected, []) --> { throw(error(expected_got_at_predicate(Expected, Inner, Loc, PredName))) }.
matcheq_expect_token_impl([C-Then | Cs], Inner, Loc, PredName, Expected, [C | X]) -->
  if_(C = Inner, Then, matcheq_expect_token_impl(Cs, Inner, Loc, PredName, Expected, X)).

triples(Tkn0, Ts0, Ts, S0, S) -->
  if_token(Tkn0, =(open_square),
    triples_blanknode(Tkn0, Ts0, Ts, S0, S),
    triples_subject(Tkn0, Ts0, Ts, S0, S)
  ).

triples_subject(Tkn0, Ts0, Ts, S0, S) -->
  subject(Sub, Tkn0, Ts0, Ts1, S0, S1),
  token(Tkn1),
  predicate_list(Sub, Tkn1, Ts1, Ts, S1, S).

triples_blanknode(Tkn0, Ts0, Ts, S0, S) -->
  blank_node_properties(Sub, Tkn0, Ts0, Ts1, S0, S1),
  token(Tkn1),
  if_token(Tkn1, =(dot),
    { Ts = Ts1, S = S1 },
    predicate_list(Sub, Tkn1, Ts1, Ts, S1, S)
  ).

predicate_list(Sub, Tkn0, Ts0, Ts, S0, S) -->
  verb(Verb, Tkn0, Ts0, Ts1, S0, S1),
  token(Tkn1),
  object_list(Sub, Verb, Tkn1, Ts1, Ts2, S1, S2),
  token(Tkn2),
  matcheq_expect_token(Tkn2, predicate_list, [
    semi  - ( token(Tkn3), predicate_list(Sub, Tkn3, Ts2, Ts, S2, S) ),
    dot   - { Ts = Ts2, S = S2 }
  ]).

object_list(Sub, Verb, Tkn0, Ts0, Ts, S0, S) -->
  object(Obj, Tkn0, Ts0, Ts1, S0, S1),
  { Ts1 = [t(Sub, Verb, Obj) | Ts2 ] },
  token(Tkn1),
  matcheq_expect_token(Tkn1, object_list, [
    comma - ( token(Tkn2), object_list(Sub, Verb, Tkn2, Ts2, Ts, S1, S) ),
    dot   - { Ts = Ts2, S = S1 }
  ]).

blank_node_properties(X, Tkn0, Ts0, Ts, S0, S) -->
  { Tkn0 = open_square }, % TODO: remove this check
  { gen_blank_node(X, S0, S1) },
  token(Tkn1),
  predicate_list(X, Tkn1, Ts0, Ts, S1, S).

% TODO: do token analisis
subject(Sub, Tkn0, Ts0, Ts, S0, S) -->
  ( { Ts = Ts0 }, iri(Sub, Tkn0, S0, S)
  ; { Ts = Ts0 }, blank_node(Sub, Tkn0, S0, S)
  ; collection(Sub, Tkn0, Ts0, Ts, S0, S)
  ).

predicate(Pred, Tkn0, S0, S) -->
  if_token(Tkn0, =(a),
    { Pred = a },
    iri(Pred, Tkn0, S0, S)
  ).

% TODO: do token analisis
object(Obj, Tkn0, Ts0, Ts, S0, S) -->
  ( { Ts = Ts0 }, iri(Obj, Tkn0, S0, S)
  ; { Ts = Ts0 }, blank_node(Obj, Tkn0, S0, S)
  ; collection(Obj, Tkn0, Ts0, Ts, S0, S)
  ; blank_node_properties(Obj, Tkn0, Ts0, Ts, S0, S)
  ; { Ts = Ts0 }, literal(Obj, Tkn0, S0, S)
  ).

iri(X, Tkn0, S0, S) -->
  % TODO
[].
blank_node(X, Tkn0, S0, S) -->
  % TODO
[].
collection(X, Tkn0, Ts0, Ts, S0, S) -->
  % TODO
[].
literal(X, Tkn0, S0, S) -->
  % TODO
[].

simple_triple(Subj, Pred, Obj) -->
  node(sub, Subj), ws_, node(pred, Pred), ws_, node(obj, Obj).

node(Kind, A, S0, S) -->
  ( { S = S0 }, iri(A)
  ; { S = S0 }, literal(A)
  ; { S = S0, state_prefix(S, SP) }, prefixed(SP, A)
  ; { state_blank(S0, S, SB0, SB) }, blank_node(A, SB0, SB) % Kind != pred
  ; { S = S0 }, collection(A)
  ).
