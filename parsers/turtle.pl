/*
 * Reference: www.w3.org/TR/turtle
**/
:- module(turtle, [
  empty_pos/1, initial_pos//0, token//1,
  empty_state/1, parse//2,
  turtledoc//4, statement//5,
  tag_type/2, tag_iri/2
]).

:- use_module(library(lists), [length/2, append/3, foldl/4]).
:- use_module(library(dcgs), []).
:- use_module(library(reif), [if_/3, (=)/3, (',')/3, (;)/3, memberd_t/3, tpartition/4]).

:- use_module(library(debug)).
$(G, A, B) :- $phrase(G, A, B).
$(G, A) :- $call(G, A).
*(_, _, _).

:- use_module(reif_dcgs, [if_//3]).

match(Elem, PredName, Cases) --> match_impl(Cases, Elem, PredName, Cases).

match_impl([], E, PredName, Cases) --> { throw(error(unreachable_match_predname(PredName, E, Cases))) }.
match_impl([If_2-Then | Cs], E, PredName, Cases) -->
  if_(call(If_2, E), Then, match_impl(Cs, E, PredName, Cases)).

:- use_module(linecol, [empty_pos/1, initial_pos//0, char//2, unchar//2]).

leq_t(A, B, T) :-
  ( var(A) -> throw(error(instantiation_error, _))
  ; var(B) -> throw(error(instantiation_error, _))
  ; number(A), number(B) ->
    ( A =< B -> T = true
    ; B < A  -> T = false
    )
  ).

between_t(A, B, N, T) :- ','(leq_t(A, N), leq_t(N, B), T).

alphanum_t(C, T) :- memberd_t(C, "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", T).
digit_t(C, T) :- memberd_t(C, "0123456789", T).
hex_t(C, T) :- memberd_t(C, "0123456789abcdefABCDEF", T).
ws_t(C, T) :- memberd_t(C, " \t\r\n", T).
quote_t(C, T) :- memberd_t(C, "\'\"", T).
number_t(C, T) :- memberd_t(C, "-+012345789", T).
exp_t(C, T) :- memberd_t(C, "eE", T).
sign_t(C, T) :- memberd_t(C, "-+", T).

ascii_control_t(C, T) :- memberd_t(C, "\x00\\x01\\x02\\x03\\x04\\x05\\x06\\x07\\x08\\x09\\x0a\\x0b\\x0c\\x0d\\x0e\\x0f\\x10\\x11\\x12\\x13\\x14\\x15\\x16\\x17\\x18\\x19\\x1a\\x1b\\x1c\\x1d\\x1e\\x1f\", T).
invalid_iriref_t(C, T) :- memberd_t(C, " <\"{}|^`", T).

sparql_prefix_t(Cs, T) :-
  length(Cs, L),
  if_(L = 6,
    ( Cs = [P, R, E, F, I, X],
      ','(
        memberd_t(P, "Pp"),
        ( memberd_t(R, "Rr"),
          memberd_t(E, "Ee"),
          memberd_t(F, "Ff"),
          memberd_t(I, "Ii"),
          memberd_t(X, "Xx")
        ),
        T
      )
    ),
    T = false
  ).
sparql_base_t(Cs, T) :-
  length(Cs, L),
  if_(L = 4,
    ( Cs = [B, A, S, E],
      ','(
        memberd_t(B, "Bb"),
        ( memberd_t(A, "Aa"),
          memberd_t(S, "Ss"),
          memberd_t(E, "Ee")
        ),
        T
      )
    ),
    T = false
  ).

pn_chars_base_t(C, T) :-
  if_(C = eof,
    T = false,
    ( char_code(C, X), pn_chars_base_t_(X, T) )
  ).
/* 6.5 [163s] */
pn_chars_base_t_(X, T) :-
  ;(
    between_t(65, 90, X),       /* char_code('A', 65), char_code('Z', 90) */
    ( between_t(97, 122, X)     /* char_code(a, 97), char_code(z, 122) */
    ; between_t(0x00C0, 0x00D6, X)
    ; between_t(0x00D8, 0x00F6, X)
    ; between_t(0x00F8, 0x02FF, X)
    ; between_t(0x0370, 0x037D, X)
    ; between_t(0x037F, 0x1FFF, X)
    ; between_t(0x200C, 0x200D, X)
    ; between_t(0x2070, 0x218F, X)
    ; between_t(0x2C00, 0x2FEF, X)
    ; between_t(0x3001, 0xD7FF, X)
    ; between_t(0xF900, 0xFDCF, X)
    ; between_t(0xFDF0, 0xFFFD, X)
    ; between_t(0x010000, 0x0EFFFF, X)
    ),
    T
  ).
/* 6.5 [164s] */
pn_chars_u_t(C, T) :- ;(pn_chars_base_t(C), C = '_', T).
pn_chars_u_t_(X, T) :- ;(pn_chars_base_t_(X), X = 95, T). /* char_code('_', 95) */
/* 6.5 [166s] */
pn_chars_t(C, T) :-
  char_code(C, X),
  ;(
    pn_chars_u_t_(X),
    ( memberd_t(C, "-0123456789\x00B7\")
    ; between_t(0x0300, 0x036F, X)
    ; between_t(0x203F, 0x2040, X)
    ),
    T
  ).
pn_local_start_t(C, T) :-
  ;(
    pn_chars_u_t(C),
    memberd_t(C, ":0123456789%\\"),
    T
  ).

eof_t(C, T) :- =(eof, C, T).
comment_t(C, T) :- =('#', C, T).

under_t(C, T) :- =('_', C, T).
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

open_square(A) -->
  char(C0, L0),
  match(C0, open_square, [
    =(' ')   - open_square(A),
    =(']')   - { A = anon },
    =(C0)    - ( { A = open_square }, unchar(C0, L0) )
  ]).

carrot(A) -->
  char(C0, L0),
  if_(C0 = (^),
    { A = double_carrot },
    ( { A = carrot }, unchar(C0, L0) )
  ).

/* 6.5 [19] [20] [21] */
number(C0, T, N) -->
  match(C0, number, [
    sign_t   - ( { N = [C0 | N1] }, number_after_sign(T, N1, []) ),
    digit_t  - ( { N = [C0 | N1] }, number_after_digit(T, N1, []) )
  ]).

number_after_sign(T, Cs0, Cs) -->
  char(C0, P0),
  match(C0, number_after_sign, [
    digit_t  - ( { Cs0 = [C0 | Cs1] }, number_after_digit(T, Cs1, Cs) ),
    dot_t    - ( noninteger_after_dot(T, P0, Cs0, Cs) ),
    exp_t    - ( { Cs0 = [C0 | Cs1], T = double }, double_after_exp(Cs1, Cs) ),
    =(C0)    - ( { Cs0 = Cs, T = integer }, unchar(C0, P0) )
  ]).

number_after_digit(T, Cs0, Cs) -->
  char(C0, P0),
  match(C0, number_after_digit, [
    digit_t  - ( { Cs0 = [C0 | Cs1] }, number_after_digit(T, Cs1, Cs) ),
    dot_t    - ( noninteger_after_dot(T, P0, Cs0, Cs) ),
    exp_t    - ( { Cs0 = [C0 | Cs1], T = double }, double_after_exp(Cs1, Cs) ),
    =(C0)    - ( { Cs0 = Cs, T = integer }, unchar(C0, P0) )
  ]).

noninteger_after_dot(T, P_1, Cs0, Cs) -->
  char(C0, P0),
  match(C0, noninteger_after_dot, [
    digit_t  - ( { Cs0 = ['.', C0 | Cs1] }, noninteger_after_dotdigit(T, Cs1, Cs) ),
    exp_t    - ( { Cs0 = ['.', C0 | Cs1], T = double }, double_after_exp(Cs1, Cs) ),
    =(C0)    - ( { Cs0 = Cs, T = integer }, unchar(C0, P0), unchar('.', P_1) )
  ]).

noninteger_after_dotdigit(T, Cs0, Cs) -->
  char(C0, P0),
  match(C0, noninteger_after_dotdigit, [
    digit_t  - ( { Cs0 = [C0 | Cs1] }, noninteger_after_dotdigit(T, Cs1, Cs) ),
    exp_t    - ( { Cs0 = [C0 | Cs1], T = double }, double_after_exp(Cs1, Cs) ),
    =(C0)    - ( { Cs0 = Cs, T = decimal }, unchar(C0, P0) )
  ]).

double_after_exp(Cs0, Cs) -->
  char(C0, P0),
  match(C0, double_after_exp, [
    sign_t   - ( { Cs0 = [C0 | Cs1] }, double_after_expsign(Cs1, Cs) ),
    digit_t  - ( { Cs0 = [C0 | Cs1] }, double_after_expdigit(Cs1, Cs) ),
    =(C0)    - { throw(error(invalid_doublechar_at(C0, P0))) }
  ]).

double_after_expsign(Cs0, Cs) -->
  char(C0, P0),
  match(C0, double_after_expsign, [
    digit_t  - ( { Cs0 = [C0 | Cs1] }, double_after_expdigit(Cs1, Cs) ),
    =(C0)    - { throw(error(invalid_doublechar_at(C0, P0))) }
  ]).

double_after_expdigit(Cs0, Cs) -->
  char(C0, P0),
  match(C0, double_after_expdigit, [
    digit_t  - ( { Cs0 = [C0 | Cs1] }, double_after_expdigit(Cs1, Cs) ),
    =(C0)    - ( { Cs0 = Cs }, unchar(C0, P0) )
  ]).

hexchar_to_num('0', 0).
hexchar_to_num('1', 1).
hexchar_to_num('2', 2).
hexchar_to_num('3', 3).
hexchar_to_num('4', 4).
hexchar_to_num('5', 5).
hexchar_to_num('6', 6).
hexchar_to_num('7', 7).
hexchar_to_num('8', 8).
hexchar_to_num('9', 9).
hexchar_to_num('a', 10).
hexchar_to_num('b', 11).
hexchar_to_num('c', 12).
hexchar_to_num('d', 13).
hexchar_to_num('e', 14).
hexchar_to_num('f', 15).
hexchar_to_num('A', 10).
hexchar_to_num('B', 11).
hexchar_to_num('C', 12).
hexchar_to_num('D', 13).
hexchar_to_num('E', 14).
hexchar_to_num('F', 15).

hex(H) -->
  char(C, P),
  { if_(hex_t(C),
      hexchar_to_num(C, H),
      throw(error(invalid_hex_at(C, P)))
    )
  }.

nibble_acc_revconcat(R, L, B) :- B is (L << 4) + R.

escape_u_len(C, Len) -->
  { length(Ns, Len) },
  foldl(hex, Ns),
  { foldl(nibble_acc_revconcat, Ns, 0, N), char_code(C, N) }.

/* 6.5 [170s] */
escape_percent(C) --> escape_u_len(C, 2).
/* 6.5 [26 : case 0] */
escape_u(C) --> escape_u_len(C, 4).
/* 6.5 [26 : case 1] */
escape_U(C) --> escape_u_len(C, 8).

escape(C) -->
  char(C0, P0),
  match(C0, escape, [
    =(t)     - { C = '\t' },
    =(b)     - { C = '\b' },
    =(n)     - { C = '\n' },
    =(r)     - { C = '\r' },
    =(f)     - { C = '\f' },
    =('\"')  - { C = '\"' },
    =('\'')  - { C = '\'' },
    =(\)     - { C = (\) },
    =('u')   - escape_u(C),
    =('U')   - escape_U(C),
    =(C0)    - { throw(error(invalid_escapechar_at(C0, P0))) }
  ]).

string_quote_quote(Q, S) -->
  char(C0, P0),
  if_(C0 = Q,
    ( string_quote3(Q, S) ),
    ( { S = [] }, unchar(C0, P0) )
  ).

string_quote(Q, S) -->
  char(C0, P0),
  if_(C0 = Q,
    string_quote_quote(Q, S),
    string_quote_(Q, C0, P0, S)
  ).

string_quote_(Q, C0, P0, S) -->
  match(C0, string_quote_, [
    eof_t    - { throw(error(unclosedstring_withsimplequote_at(Q, P0))) },
    =('\r')  - { throw(error(invalid_simplequotechar_at(C0, P0))) },
    =('\n')  - { throw(error(invalid_simplequotechar_at(C0, P0))) },
    =(Q)     - { S = [] },
    =(\)     - ( { S = [E0 | S1] }, escape(E0), char(C1, P1), string_quote_(Q, C1, P1, S1) ),
    =(C0)    - ( { S = [C0 | S1] }, char(C1, P1), string_quote_(Q, C1, P1, S1) )
  ]).

string_quote3_quote(Q, S) -->
  char(C0, P0),
  if_(C0 = Q,
    string_quote3_quote_quote(Q, S),
    ( { S = [Q | S0] }, string_quote3_(Q, C0, P0, S0) )
  ).

string_quote3_quote_quote(Q, S) -->
  char(C0, P0),
  if_(C0 = Q,
    { S = [] },
    ( { S = [Q, Q | S0] }, string_quote3_(Q, C0, P0, S0) )
  ).

string_quote3(Q, S) -->
  char(C0, P0),
  string_quote3_(Q, C0, P0, S).

string_quote3_(Q, C0, P0, S) -->
  match(C0, string_quote3_, [
    eof_t    - { throw(error(unclosedstring_withlongquote_at(P0))) },
    =(Q)     - string_quote3_quote(Q, S),
    =(\)     - ( { S = [E0 | S1] }, escape(E0), char(C1, P1), string_quote3_(Q, C1, P1, S1) ),
    =(C0)    - ( { S = [C0 | S1] }, char(C1, P1), string_quote3_(Q, C1, P1, S1) )
  ]).

iriref(R) -->
  char(C0, P0),
  match(C0, iriref, [
    eof_t            - { throw(error(unclosediri_at(P0))) },
    ascii_control_t  - { throw(error(invalid_irirefchar_at(C0, P0))) },
    invalid_iriref_t - { throw(error(invalid_irirefchar_at(C0, P0))) },
    =('>')           - { R = [] },
    =(\)             - iriref_escape(R),
    =(C0)            - ( { R = [C0 | R1] }, iriref(R1) )
  ]).

iriref_escape(R) -->
  char(C0, P0),
  match(C0, iriref_escape, [
    =('u')   - ( { R = [C | R1] }, escape_u(C), iriref(R1) ),
    =('U')   - ( { R = [C | R1] }, escape_U(C), iriref(R1) ),
    =(C0)    - { throw(error(invalid_irirefescapechar_at(C0, P0))) }
  ]).

/* 6.5 [139s] */
pname_ns(C0, P0, Tag, N) -->
  match(C0, pname_ns, [
    =(':')          - { Tag = ns, N = [] },
    pn_chars_base_t - ( { N = [C0 | N1] }, pname_ns_after(Tag, N1) ),
    =(C0)           - { throw(error(invalid_namespacechar_at(C0, P0))) }
  ]).

pname_ns_after(Tag, N) -->
  char(C0, P0),
  match(C0, pname_ns_after, [
    eof_t      - ( { Tag = id, N = [] }, unchar(C0, P0) ),
    =(':')     - { Tag = ns, N = [] },
    =('.')     - ( { N = [C0 | N1] }, pname_ns_dot(Tag, N1) ),
    pn_chars_t - ( { N = [C0 | N1] }, pname_ns_after(Tag, N1) ),
    =(C0)      - ( { Tag = id, N = [] }, unchar(C0, P0) )
  ]).

pname_ns_dot(Tag, N) -->
  char(C0, P0),
  match(C0, pname_ns_dot, [
    eof_t      - ( { Tag = id, N = [] }, unchar(C0, P0) ),
    =(':')     - { throw(error(invalid_namespace_endswithdot_at(P0))) },
    =('.')     - ( { N = [C0 | N1] }, pname_ns_dot(Tag, N1) ),
    pn_chars_t - ( { N = [C0 | N1] }, pname_ns_after(Tag, N1) ),
    =(C0)      - { throw(error(invalid_namespacechar_at(C0, P0))) }
  ]).

/* 6.5 [172s] */
pn_local_escape(C) -->
  char(C0, P0),
  match(C0, pn_local_escape, [
    =('_')     - { C = '_' },
    =(~)       - { C = (~) },
    =('.')     - { C = '.' },
    =(-)       - { C = (-) },
    =(!)       - { C = (!) },
    =($)       - { C = ($) },
    =(&)       - { C = (&) },
    =('\'')    - { C = '\'' },
    =('(')     - { C = '(' },
    =(')')     - { C = ')' },
    =(*)       - { C = (*) },
    =(+)       - { C = (+) },
    =((','))   - { C = (',') },
    =(;)       - { C = (;) },
    =('=')     - { C = (=) },
    =(/)       - { C = (/) },
    =('?')     - { C = '?' },
    =('#')     - { C = '#' },
    =('@')     - { C = '@' },
    =('%')     - { C = '%' },
    =(C0)      - { throw(error(invalid_escapechar_at(C0, P0))) }
  ]).

/* 6.5 [168s] */
pn_local_(C0, P0, L) -->
  match(C0, pn_local_, [
    pn_chars_u_t   - ( { L = [C0 | L1] }, pn_local_after(L1) ),
    digit_t        - ( { L = [C0 | L1] }, pn_local_after(L1) ),
    =(':')         - ( { L = [C0 | L1] }, pn_local_after(L1) ),
    =('%')         - ( { L = [C | L1] }, escape_percent(C), pn_local_after(L1) ),
    =(\)           - ( { L = [C | L1] }, pn_local_escape(C), pn_local_after(L1) ),
    =(C0)          - { throw(error(invalid_localchar_at(C0, P0))) }
  ]).

pn_local_after(L) -->
  char(C0, P0),
  match(C0, pn_local_after, [
    eof_t      - ( { L = [] }, unchar(C0, P0) ),
    =('.')     - pn_local_dot(P0, L),
    =(':')     - ( { L = [C0 | L1] }, pn_local_after(L1) ),
    pn_chars_t - ( { L = [C0 | L1] }, pn_local_after(L1) ),
    =('%')     - ( { L = [C | L1] }, escape_percent(C), pn_local_after(L1) ),
    =(\)       - ( { L = [C | L1] }, pn_local_escape(C), pn_local_after(L1) ),
    =(C0)      - ( { L = [] }, unchar(C0, P0) )
  ]).

pn_local_dot(P_1, L) -->
  char(C0, P0),
  match(C0, pn_local_dot, [
    eof_t      - ( { L = [] }, unchar(C0, P0), unchar('.', P_1) ),
    =('.')     - ( { L = ['.' | L1] }, pn_local_dot(P0, L1) ),
    =(':')     - ( { L = ['.', C0 | L1] }, pn_local_after(L1) ),
    pn_chars_t - ( { L = ['.', C0 | L1] }, pn_local_after(L1) ),
    =('%')     - ( { L = ['.', C | L1] }, espace_percent(C), pn_local_after(L1) ),
    =(\)       - ( { L = ['.', C | L1] }, pn_local_escape(C), pn_local_after(L1) ),
    =(C0)      - ( { L = [] }, unchar(C0, P0), unchar('.', P_1) )
  ]).

langtag(L) -->
  char(C0, P0),
  match(C0, langtag, [
    alphanum_t - ( { L = [C0 | L1] }, langtag_after(L1) ),
    =(C0)      - { throw(error(invalid_langtagchar_at(C0, P0))) }
  ]).

langtag_after(L) -->
  char(C0, P0),
  match(C0, langtag_after, [
    alphanum_t - ( { L = [C0 | L1] }, langtag_after(L1) ),
    =('-')     - ( { L = [C0 | L1] }, langtag_after(L1) ),
    =(C0)      - ( { L = [] }, unchar(C0, P0) )
  ]).

id(C0, P0, tkn(P0, T)) -->
  pname_ns(C0, P0, Tag, N),
  id_classify(Tag, N, T).

id_classify(ns, N, T) -->
  char(C1, P1),
  if_(pn_local_start_t(C1),
    ( { T = prefixed(N, L) }, pn_local_(C1, P1, L) ),
    ( { T = namespace(N) }, unchar(C1, P1) )
  ).
id_classify(id, N, T) -->
  match(N, id_classify_id, [
    sparql_prefix_t   - { T = sparql_prefix },
    sparql_base_t     - { T = sparql_base },
    =("false")        - { T = boolean("false") },
    =("true")         - { T = boolean("true") },
    =("a")            - { T = a },
    =(N)              - { T = id(N) }
  ]).

token_(P, T) --> token__(P, T). token__(_, T) --> token(T).

token(T) -->
  char(C0, L0),
  match(C0, token, [
    ws_t      - token(T),
    eof_t     - { T = tkn(L0, eof) },
    comment_t - ( comment(_), token(T) ),
    under_t   - { T = tkn(L0, underscore) },
    comma_t   - { T = tkn(L0, comma) },
    semi_t    - { T = tkn(L0, semi) },
    dot_t     - { T = tkn(L0, dot) },
    at_t      - ( { T = tkn(L0, langtag(L)) }, langtag(L) ),
    =('(')    - { T = tkn(L0, open_par) },
    =(')')    - { T = tkn(L0, close_par) },
    =('[')    - ( { T = tkn(L0, A) }, open_square(A) ),
    =(']')    - { T = tkn(L0, close_square) },
    =('^')    - ( { T = tkn(L0, A) }, carrot(A) ),
    =('<')    - ( { T = tkn(L0, iriref(R)) }, iriref(R) ),
    quote_t   - ( { T = tkn(L0, string(S)) }, string_quote(C0, S) ),
    number_t  - ( { T = tkn(L0, number(K, N)) }, number(C0, K, N) ),
    =(C0)     - id(C0, L0, T)
  ]).

if_token(tkn(_, Inner), Match_2, Then_2, Else_2) :-
  if_(call(Match_2, Inner), Then_2, Else_2).

if_token(tkn(_, Inner), Match_2, Then_2, Else_2) -->
  if_(call(Match_2, Inner), Then_2, Else_2).

match_expect_token(tkn(Loc, Inner), PredName, Cases) --> match_expect_token_impl(Cases, Inner, Loc, PredName, X, X).

match_expect_token_impl([], Inner, Loc, PredName, Expected, []) --> { throw(error(expected_got_at_predicate(Expected, Inner, Loc, PredName))) }.
match_expect_token_impl([If_2-Then | Cs], Inner, Loc, PredName, Expected, [If_2 | X]) -->
  if_(call(If_2, Inner), Then, match_expect_token_impl(Cs, Inner, Loc, PredName, Expected, X)).

matcheq_expect_token(tkn(Loc, Inner), PredName, Cases) :- matcheq_expect_token_impl(Cases, Inner, Loc, PredName, X, X).

matcheq_expect_token_impl([], Inner, Loc, PredName, Expected, []) :- throw(error(expected_got_at_predicate(Expected, Inner, Loc, PredName))).
matcheq_expect_token_impl([C-Then | Cs], Inner, Loc, PredName, Expected, [C | X]) :-
  if_(C = Inner, Then, matcheq_expect_token_impl(Cs, Inner, Loc, PredName, Expected, X)).

matcheq_expect_token(tkn(Loc, Inner), PredName, Cases) --> matcheq_expect_token_impl(Cases, Inner, Loc, PredName, X, X).

matcheq_expect_token_impl([], Inner, Loc, PredName, Expected, []) --> { throw(error(expected_got_at_predicate(Expected, Inner, Loc, PredName))) }.
matcheq_expect_token_impl([C-Then | Cs], Inner, Loc, PredName, Expected, [C | X]) -->
  if_(C = Inner, Then, matcheq_expect_token_impl(Cs, Inner, Loc, PredName, Expected, X)).

%%%%%%%%%%%%%%%%%%%% Parser %%%%%%%%%%%%%%%%%%%%

tag_type(boolean, 'http://www.w3.org/2001/XMLSchema#boolean').
tag_type(integer, 'http://www.w3.org/2001/XMLSchema#integer').
tag_type(decimal, 'http://www.w3.org/2001/XMLSchema#decimal').
tag_type(double, 'http://www.w3.org/2001/XMLSchema#double').
tag_type(string, 'http://www.w3.org/2001/XMLSchema#string').
tag_type(lang_string, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString').
tag_type(a, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type').

tag_iri(Tag, Iri) :- tag_type(Tag, Ty), iri_atom(Iri, Ty).

empty_state(ps_b_b([], [], 0)).

namespace_state(Ns0, Ns, ps_b_b(Ns0, B, G), ps_b_b(Ns, B, G)).
base_state(B0, B, ps_b_b(Ns, B0, G), ps_b_b(Ns, B, G)).
gen_state(G0, G, ps_b_b(Ns, B, G0), ps_b_b(Ns, B, G)).

keyeq_t(K, K1-_, T) :- =(K, K1, T).
cons_prefix_state(N, P, ps_b_b(Ns0, B, G), ps_b_b([N-P | Ns1], B, G)) :-
  tpartition(keyeq_t(N), Ns0, _, Ns1).

iri_atom(resource(iri, A), A).
iri_chars(Iri, Str) :- atom_chars(A, Str), iri_atom(Iri, A).

append_base(ps_b_b(_, B, _), R, X) :-
  % TODO: smartter checking
  append(B, R, Xs),
  iri_chars(X, Xs).
append_prefix(ps_b_b(Ns, _, _), N, R, X) :-
  if_(memberd_t(N-P, Ns),
    ( append(P, R, Xs), iri_chars(X, Xs) ),
    throw(error(prefix_not_defined(N)))
  ).
gen_blanknode(ps_b_b(Ns, B, G0), ps_b_b(Ns, B, G), resource(blank(unlabeled), G0)) :-
  G is G0 + 1.

parse(Ts, S) --> { empty_state(S0) }, initial_pos, turtledoc(Ts, [], S0, S).

/* 6.5 [1] */
turtledoc(Ts0, Ts, S0, S) -->
  token_(turtledoc, Tkn0),
  if_token(Tkn0, =(eof),
    { Ts = Ts0, S = S0 },
    ( statement(Tkn0, Ts0, Ts1, S0, S1), turtledoc(Ts1, Ts, S1, S) )
  ).

/* 6.5 [2] */
statement(Tkn0, Ts0, Ts, S0, S) -->
  match_expect_token(Tkn0, statement, [
    /* 6.5 [3 : case 0] (inlined) [2 : case 0] */
    =(langtag("prefix"))  - ( { Ts = Ts0 }, prefixid(S0, S) ),
    /* 6.5 [3 : case 1] (inlined) [2 : case 0] */
    =(langtag("base"))    - ( { Ts = Ts0 }, base(S0, S) ),
    /* 6.5 [3 : case 2] (inlined) [2 : case 0] */
    =(sparql_prefix)      - ( { Ts = Ts0 }, prefixid_(S0, S) ),
    /* 6.5 [3 : case 3] (inlined) [2 : case 0] */
    =(sparql_base)        - ( { Ts = Ts0 }, base_(S0, S) ),
    /* 6.5 [2 : case 1] */
    triples_t             - triples(Tkn0, Ts0, Ts, S0, S)
  ]).

/* 6.5 [4] */
prefixid(S0, S) -->
  prefixid_(S0, S),
  token_(prefixid, Tkn0),
  matcheq_expect_token(Tkn0, prefixid, [
    dot   - []
  ]).

/* 6.5 [5] */
base(S0, S) -->
  base_(S0, S),
  token_(base, Tkn0),
  matcheq_expect_token(Tkn0, base, [
    dot   - []
  ]).

/* 6.5 [5s] */
base_(S0, S) -->
  token_(base_, Tkn0),
  matcheq_expect_token(Tkn0, base_, [
    iriref(B)   - { base_state(_, B, S0, S) }
  ]).

/* 6.5 [6s] */
prefixid_(S0, S) -->
  token_(prefixid_namespace, Tkn0),
  matcheq_expect_token(Tkn0, prefixid_, [
    namespace(N)    - []
  ]),
  token_(prefixid_iriref, Tkn1),
  matcheq_expect_token(Tkn1, prefixid_, [
    iriref(P)       - { cons_prefix_state(N, P, S0, S) }
  ]).

/* 6.5 [6] */
triples_t(Tkn, T) :- ';'(iri_t(Tkn), memberd_t(Tkn, [open_par, open_square]), T).
triples(Tkn0, Ts0, Ts, S0, S) -->
  if_token(Tkn0, =(open_square),
    triples_blanknode(Tkn, Ts0, Ts, S0, S),
    triples_subject(Tkn0, Tkn, Ts0, Ts, S0, S)
  ),
  matcheq_expect_token(Tkn, triples, [dot-[]]).

/* 6.5 [6 : case 0] */
triples_subject(Tkn0, Tkn, Ts0, Ts, S0, S) -->
  subject(Sub, Tkn0, Ts0, Ts1, S0, S1),
  token_(triples_subject, Tkn1),
  predicate_list(Sub, Tkn1, Tkn, Ts1, Ts, S1, S).

/* 6.5 [6 : case 1] */
triples_blanknode(Tkn, Ts0, Ts, S0, S) -->
  blank_node_property_list(Sub, Ts0, Ts1, S0, S1),
  token_(triples_blanknode, Tkn1),
  if_token(Tkn1, =(dot),
    { Tkn = Tkn1, Ts = Ts1, S = S1 },
    predicate_list(Sub, Tkn1, Tkn, Ts1, Ts, S1, S)
  ).

/* 6.5 [7] */
predicate_list(Sub, Tkn0, Tkn, Ts0, Ts, S0, S) -->
  { Ts0 = Ts1, S0 = S1, verb(Verb, Tkn0, S0) },
  token_(predicate_list, Tkn1),
  object_list(Sub, Verb, Tkn1, Tkn2, Ts1, Ts2, S1, S2),
  matcheq_expect_token(Tkn2, predicate_list, [
    semi          - ( token_(predicate_list_semi, Tkn3), predicate_list(Sub, Tkn3, Tkn, Ts2, Ts, S2, S) ),
    close_square  - { Tkn = Tkn2, Ts = Ts2, S = S2 },
    dot           - { Tkn = Tkn2, Ts = Ts2, S = S2 }
  ]).

/* 6.5 [8] */
object_list(Sub, Verb, Tkn0, Tkn, Ts0, Ts, S0, S) -->
  object(Obj, Tkn0, Tkn1, Ts0, Ts1, S0, S1),
  { Ts1 = [t(Sub, Verb, Obj) | Ts2] },
  if_token(Tkn1, =(comma),
    ( token_(object_list_comma, Tkn2), object_list(Sub, Verb, Tkn2, Tkn, Ts2, Ts, S1, S) ),
    { Tkn1 = Tkn, Ts = Ts2, S = S1 }
  ).

/* 6.5 [9] */
verb(Pred, Tkn0, S) :-
  if_token(Tkn0, =(a),
    /* 6.5 [9 : case 1] */
    { tag_type(a, Pred) },
    /* 6.5 [11] (inlined) [9 : case 0] */
    iri(Pred, Tkn0, S)
  ).

/* 6.5 [10] */
subject(Sub, Tkn0, Ts0, Ts, S0, S) -->
  match_expect_token(Tkn0, subject, [
    /* 6.5 [10 : case 0] */
    iri_t         - { Ts = Ts0, S = S0, iri(Sub, Tkn0, S0) },
    /* 6.5 [10 : case 1] */
    blank_node_t  - { Ts = Ts0, blank_node(Sub, Tkn0, S0, S) },
    /* 6.5 [10 : case 2] */
    =(open_par)   - collection(Sub, Tkn0, Ts0, Ts, S0, S)
  ]).

/* 6.5 [12] */
object_follow_t(Tkn, T) :- memberd_t(Tkn, [dot, semi, comma, close_par, close_square], T).
object(Obj, Tkn0, Tkn, Ts0, Ts, S0, S) -->
  match_expect_token(Tkn0, object, [
    /* 6.5 [12 : case 0] */
    iri_t           - ( { Ts = Ts0, S = S0, iri(Obj, Tkn0, S0) }, token_(object_iri, Tkn) ),
    /* 6.5 [12 : case 1] */
    blank_node_t    - ( { Ts = Ts0 }, blank_node(Obj, Tkn0, S0, S), token_(object_blank_node, Tkn) ),
    /* 6.5 [12 : case 2] */
    =(open_par)     - ( collection(Obj, Tkn0, Ts0, Ts, S0, S), token_(object_collection, Tkn) ),
    /* 6.5 [12 : case 3] */
    =(open_square)  - ( blank_node_property_list(Obj, Ts0, Ts, S0, S), token_(object_blanknodepropertylist, Tkn) ),
    /* 6.5 [12 : case 4] */
    literal_t       - ( { Ts = Ts0, S = S0 }, literal(Obj, Tkn0, Tkn, S) )
  ]).

/* 6.5 [13] */
literal_t(Tkn, T) :- memberd_t(Tkn, [string(_), number(_, _), boolean(_)], T).
literal(X, Tkn0, Tkn, S) -->
  matcheq_expect_token(Tkn0, literal, [
    /* 6.5 [17] (inlined in tokenizer) [13 : case 0] */
    string(Str)   - ( token_(literal_string, Tkn1), rdf_literal(X, Str, Tkn1, Tkn, S) ),
    /* 6.5 [16] (inlined in tokenizer) [13 : case 1] */
    number(T, N)  - ( { tag_iri(T, Ty), X = literal(Ty, N) }, token_(literal_number, Tkn) ),
    /* 6.5 [133s] (inlined in tokenizer) [13 : case 2] */
    boolean(B)    - ( { tag_iri(boolean, Ty), X = literal(Ty, B) }, token_(literal_boolean, Tkn) )
  ]).

/* 6.5 [14] */
blank_node_property_list(X, Ts0, Ts, S0, S) -->
  { gen_blanknode(S0, S1, X) },
  token_(blank_node_property_list, Tkn0),
  predicate_list(X, Tkn0, Tkn, Ts0, Ts, S1, S),
  matcheq_expect_token(Tkn, blank_node_property_list, [close_square-[]]).

/* 6.5 [15] */
collection(X, Tkn0, Ts0, Ts, S0, S) -->
  % TODO
[].

/* 6.5 [128s] */
rdf_literal(X, Str, Tkn0, Tkn, S) -->
  match_expect_token(Tkn0, rdf_literal, [
    /* 6.5 [128s : case 0] */
    =(langtag(L))     - ( { tag_iri(lang_string, Ty), X = literal(Ty, @(Str, L)) }, token_(rdf_literal_langtag, Tkn) ),
    /* 6.5 [128s : case 1] */
    =(double_carrot)  - ( { X = literal(Ty, Str) }, token_(rdf_literal_double_carrot, Tkn1), { iri(Ty, Tkn1, S) }, token_(rdf_literal_iri, Tkn) ),
    /* 6.5 [128s : case failed_optional] */
    object_follow_t   - { tag_iri(string, Ty), X = literal(Ty, Str), Tkn0 = Tkn }
  ]).

/* 6.5 [135s] */
iri_t(Tkn, T) :- memberd_t(Tkn, [iriref(_), prefixed(_, _), namespace(_)], T).
iri(X, Tkn0, S) :-
  matcheq_expect_token(Tkn0, iri, [
    iriref(R)       - append_base(S, R, X),
    /* 6.5 [136s] (inlined) */
    prefixed(P, L)  - append_prefix(S, P, L, X),
    namespace(N)    - append_prefix(S, N, [], X)
  ]).

/* 6.5 [137s] */
blank_node_t(Tkn, T) :- memberd_t(Tkn, [], T).
blank_node(X, Tkn0, S0, S) -->
  % TODO
[].
