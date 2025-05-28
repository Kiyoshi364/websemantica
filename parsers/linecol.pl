:- module(linecol, [
  pos_line/2, pos_col/2, pos_byte/2,
  empty_pos/1, initial_pos//0,
  loc//1,
  char//1, char//2, unchar//2
]).

:- use_module(library(dcgs), []).
:- use_module(library(reif), [if_/3, (=)/3]).
:- use_module(library(freeze), [freeze/2]).

pos_line(pos( L, _C, _B), L).
pos_col( pos(_L,  C, _B), C).
pos_byte(pos(_L, _C,  B), B).

pos_ground(P) :-
  nonvar(P),
  functor(P, pos, 3),
  pos_line(P, L), number(L),
  pos_col( P, C), number(C),
  pos_byte(P, B), number(B),
true.

empty_pos(pos(0, 0, 0)).

initial_pos, [P] --> { empty_pos(P) }.
final_pos(P) --> [P].

loc(L), [L] --> [L].

char(C) --> char(C, _).
char(C0, P0), [P] --> [P0, C0], !, { char_pos(C0, P0, P) }.
char(C0, P0) --> [P0], { C0 = eof }.

unchar(eof, P, [], S) :- !, S = [P].
unchar(C, P), [P, C] --> [P0], { char_pos(C, P, P0) }.

char_pos(X, P0, P) :-
  ( pos_ground(P0) -> char_pos_forward(X, P0, P)
  ; pos_ground(P ) -> char_pos_backward(X, P, P0)
  ; freeze(P0, char_pos_forward(X, P0, P)),
    freeze(P , char_pos_backward(X, P, P0))
  ).

char_pos_forward(X, P0, P) :-
  pos_line(P0, L0),
  pos_col( P0, C0),
  pos_byte(P0, B0),
  if_(X = '\n',
    ( L is L0 + 1, C is 0, B is B0 + 1 ),
    ( char_code(X, Code),
      % TODO: handle Inc better
      ( Code < 0x100 ->
        Inc = 1
      ; Code < 0x10000 ->
        Inc = 2
      ; Code < 0x1000000 ->
        Inc = 3
      ; Inc = 4
      ),
      L is L0, C is C0 + 1, B is B0 + Inc )
  ),
  pos_line(Pa, L),
  pos_col( Pa, C),
  pos_byte(Pa, B),
  P = Pa.

char_pos_backward(X, P, P0) :-
  pos_line(P, L),
  pos_col( P, C),
  pos_byte(P, B),
  if_(X = '\n',
    ( L0 is L - 1, C is 0 ),
    ( char_code(X, Code), Inc is div(Code, 0x100) + 1,
      L0 is L, C0 is C - Inc )
  ),
  B0 is B - 1,
  pos_line(P0a, L0),
  pos_col( P0a, C0),
  pos_byte(P0a, B0),
  P0 = P0a.
