parent_child(sergio, milton).       parent_child(lara, milton).
parent_child(milton, george).       parent_child(milton, carmem).
parent_child(helena, george).       parent_child(helena, carmem).
parent_child(george, ana).          parent_child(george, andre).
parent_child(mario, maria).         parent_child(dora, maria).
parent_child(maria, ana).           parent_child(maria, andre).
parent_child(carmem, sara).         parent_child(carmem, ema).
parent_child(alexandre, sara).      parent_child(alexandre, ema).

male(sergio).       male(milton).       male(george).
male(mario).        male(alexandre).    male(andre).

female(lara).       female(carmem).     female(helena).
female(dora).       female(maria).      female(ana).
female(sara).       female(ema).

asc_desc(A, D) :-
  parent_child(A, X),
  ( X = D ; asc_desc(X, D) ).

asc_desc(A, D, P) :-
  parent_child(A, X),
  ( P = [], X = D ; P = [X | P1], asc_desc(X, D, P1) ).
