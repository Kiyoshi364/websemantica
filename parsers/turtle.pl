/*
 * Reference: www.w3.org/TR/turtle
**/
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

ws_t(C, T) :- memberd_t(C, " \t\r\n", T).
quote_t(C, T) :- memberd_t(C, "\'\"", T).

eof_t(C, T) :- =(eof, C, T).
comment_t(C, T) :- =('#', C, T).

under_t(C, T) :- =('_', C, T).
colon_t(C, T) :- =(':', C, T).
comma_t(C, T) :- =(',', C, T).
semi_t(C, T) :- =(';', C, T).
dot_t(C, T) :- =('.', C, T).
at_t(C, T) :- =('@', C, T).

comment(Comment) -->
  char(C, P),
  if_(C = '\n',
    ( { Comment = [] }, unchar(C, P) ),
    if_(C = eof,
      { Comment = [] },
      ( { Comment = [C|T] }, comment(T) )
    )
  ).

string(C0, L0, tkn(L0, string(C0))) -->
  % TODO
[].

id(C0, L0, tkn(L0, id(C0))) --> [].

token(T) -->
  char(C0, L0),
  matcheq(C0, [
    ws_t      - token(T),
    eof_t     - { T = eof },
    comment_t - ( { T = tkn(L0, comment(C)) }, comment(C) ),
    under_t   - { T = tkn(L0, underscore) },
    colon_t   - { T = tkn(L0, colon) },
    comma_t   - { T = tkn(L0, comma) },
    semi_t    - { T = tkn(L0, semi) },
    dot_t     - { T = tkn(L0, dot) },
    at_t      - { T = tkn(L0, at) },
    =('(')    - { T = tkn(L0, open_par) },
    =(')')    - { T = tkn(L0, close_par) },
    =('[')    - { T = tkn(L0, open_square) },
    =(']')    - { T = tkn(L0, close_square) },
    =('<')    - { T = tkn(L0, open_angle) },
    =('>')    - { T = tkn(L0, close_angle) },
    quote_t   - string(C0, L0, T),
    =(C0 )    - id(C0, L0, T)
  ]).

if_token(tkn(_, Inner), Match_2, Then_2, Else_2) -->
  if_(call(Match_2, Inner), Then_2, Else_2).

matcheq_expect_token(tkn(Loc, Inner), PredName, Cases) --> matcheq_expect_token_impl(Cases, Inner, Loc, PredName, X, X).

matcheq_expect_token_impl([], Inner, Loc, PredName, Expected, []) --> { throw(error(expected_got_at_predicate(Expected, Inner, Loc, PredName))) }.
matcheq_expect_token_impl([C-Then | Cs], Inner, Loc, PredName, Expected, [C | X]) -->
  if_(C = Inner, Then, matcheq_expect_token_impl(Cs, Inner, Loc, PredName, Expected, X)).

%%%%%%%%%%%%%%%%%%%% Parser %%%%%%%%%%%%%%%%%%%%

/* TODO
 * 6.5 [1]
 * 6.5 [2]
 * 6.5 [3]
 * 6.5 [4]
 * 6.5 [5]
 * 6.5 [5s]
 * 6.5 [6s]
**/

/* 6.5 [6] */
triples(Tkn0, Ts0, Ts, S0, S) -->
  if_token(Tkn0, =(open_square),
    triples_blanknode(Tkn0, Ts0, Ts, S0, S),
    triples_subject(Tkn0, Ts0, Ts, S0, S)
  ).

/* 6.5 [6 : case 0] */
triples_subject(Tkn0, Ts0, Ts, S0, S) -->
  subject(Sub, Tkn0, Ts0, Ts1, S0, S1),
  token(Tkn1),
  predicate_list(Sub, Tkn1, Ts1, Ts, S1, S).

/* 6.5 [6 : case 1] */
triples_blanknode(Tkn0, Ts0, Ts, S0, S) -->
  blank_node_properties(Sub, Tkn0, Ts0, Ts1, S0, S1),
  token(Tkn1),
  if_token(Tkn1, =(dot),
    { Ts = Ts1, S = S1 },
    predicate_list(Sub, Tkn1, Ts1, Ts, S1, S)
  ).

/* 6.5 [7] */
predicate_list(Sub, Tkn0, Ts0, Ts, S0, S) -->
  verb(Verb, Tkn0, Ts0, Ts1, S0, S1),
  token(Tkn1),
  object_list(Sub, Verb, Tkn1, Ts1, Ts2, S1, S2),
  token(Tkn2),
  matcheq_expect_token(Tkn2, predicate_list, [
    semi  - ( token(Tkn3), predicate_list(Sub, Tkn3, Ts2, Ts, S2, S) ),
    dot   - { Ts = Ts2, S = S2 }
  ]).

/* 6.5 [8] */
object_list(Sub, Verb, Tkn0, Ts0, Ts, S0, S) -->
  object(Obj, Tkn0, Ts0, Ts1, S0, S1),
  { Ts1 = [t(Sub, Verb, Obj) | Ts2 ] },
  token(Tkn1),
  matcheq_expect_token(Tkn1, object_list, [
    comma - ( token(Tkn2), object_list(Sub, Verb, Tkn2, Ts2, Ts, S1, S) ),
    dot   - { Ts = Ts2, S = S1 }
  ]).

/* 6.5 [9] */
verb(Pred, Tkn0, S0, S) -->
  if_token(Tkn0, =(id(a)),
    /* 6.5 [9 case 1] */
    { Pred = a },
    /* 6.5 [11] (inlined) [9 case 0] */
    iri(Pred, Tkn0, S0, S)
  ).

/* 6.5 [10] */
% TODO: do token analisis
subject(Sub, Tkn0, Ts0, Ts, S0, S) -->
  ( { Ts = Ts0 }, iri(Sub, Tkn0, S0, S)
  ; { Ts = Ts0 }, blank_node(Sub, Tkn0, S0, S)
  ; collection(Sub, Tkn0, Ts0, Ts, S0, S)
  ).

/* 6.5 [12] */
% TODO: do token analisis
object(Obj, Tkn0, Ts0, Ts, S0, S) -->
  /* 6.5 [12 case 0] */
  ( { Ts = Ts0 }, iri(Obj, Tkn0, S0, S)
  /* 6.5 [12 case 1] */
  ; { Ts = Ts0 }, blank_node(Obj, Tkn0, S0, S)
  /* 6.5 [12 case 2] */
  ; collection(Obj, Tkn0, Ts0, Ts, S0, S)
  /* 6.5 [12 case 3] */
  ; blank_node_properties(Obj, Tkn0, Ts0, Ts, S0, S)
  /* 6.5 [12 case 4] */
  ; { Ts = Ts0 }, literal(Obj, Tkn0, S0, S)
  ).

/* 6.5 [13] */
literal(X, Tkn0, Tkn) -->
  matcheq_expect_token(Tkn0, literal, [
    /* 6.5 [13 case 0] */
    string(Str) - ( token(Tkn1), rdf_literal(X, Str, Tkn1, Tkn) ),
    /* 6.5 [16] (inlined) [13 case 1] */
    number(N)   - ( { X = literal(number, N) }, token(Tkn) )
    /* 6.5 [133s] (inlined) [13 case 2] */
    boolean(B)  - ( { X = literal(boolean, B) }, token(Tkn) )
  ]).

/* 6.5 [14] */
blank_node_properties(X, Tkn0, Ts0, Ts, S0, S) -->
  { Tkn0 = open_square }, % TODO: remove this check
  { gen_blank_node(X, S0, S1) },
  token(Tkn1),
  predicate_list(X, Tkn1, Ts0, Ts, S1, S).

/* 6.5 [15] */
collection(X, Tkn0, Tkn, Ts0, Ts, S0, S) -->
  % TODO
[].

/* 6.5 [128s] */
rdf_literal(X, Str, Tkn0, Tkn) -->
  /* 6.5 [17] (inlined) */
  % TODO
[].

/* 6.5 [135s] */
iri(X, Tkn0, S0, S) -->
  /* 6.5 [136s] (inlined) */
  % TODO
[].

/* 6.5 [137s] */
blank_node(X, Tkn0, S0, S) -->
  % TODO
[].
