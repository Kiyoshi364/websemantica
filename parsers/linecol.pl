:- module(linecol, [
  pos/3, pos/5, empty_pos/1, initial_pos//0,
  char//1, unchar//1
]).

:- use_module(library(dcgs), []).
:- use_module(library(reif), [if_/3, (=)/3]).

pos(K, V, S) :- pos(K, V, V, S, S).

pos(line, L0, L, pos(L0, C , B ), pos(L, C, B)).
pos(col , C0, C, pos(L , C0, B ), pos(L, C, B)).
pos(byte, B0, B, pos(L , C , B0), pos(L, C, B)).

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

char(C), [P] --> [P0, C], { char_pos(C, P0, P) }.

unchar(C), [P, C] --> [P0], { char_pos(C, P, P0) }.

char_pos(X, P0, P) :-
  ( pos_ground(P0) -> char_pos_forward(X, P0, P)
  ; pos_ground(P ) -> char_pos_backward(X, P, P0)
  ; freeze(P0, char_pos_forward(X, P0, P)),
    freeze(P , char_pos_backward(X, P, P0))
  ).

char_pos_forward(X, P0, P) :-
  pos(line, L0, P0),
  pos(col , C0, P0),
  pos(byte, B0, P0),
  if_(X = '\n',
    ( L is L0 + 1, C is 0 ),
    ( char_code(X, Code), Inc is div(Code, 0x100) + 1,
      L is L0, C is C0 + Inc )
  ),
  B is B0 + 1,
  pos(line, L, P),
  pos(col , C, P),
  pos(byte, B, P).

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
  pos_line(P0, L0),
  pos_col( P0, C0),
  pos_byte(P0, B0).
