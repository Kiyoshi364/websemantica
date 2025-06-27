pai_filho(sergio, milton).
pai_filho(lara, milton).
pai_filho(milton, george).
pai_filho(milton, carmem).
pai_filho(helena, george).
pai_filho(helena, carmem).
pai_filho(george, ana).
pai_filho(george, andre).
pai_filho(mario, maria).
pai_filho(dora, maria).
pai_filho(maria, ana).
pai_filho(maria, andre).
pai_filho(carmem, sara).
pai_filho(carmem, ema).
pai_filho(alexandre, sara).
pai_filho(alexandre, ema).

homem(sergio).
homem(milton).
homem(george).
homem(mario).
homem(alexandre).
homem(andre).

mulher(carmem).
mulher(helena).
mulher(dora).
mulher(maria).
mulher(ana).
mulher(sara).
mulher(ema).

avo_neto(A, N) :-
  pai_filho(A, X),
  pai_filho(X, N).

asc_desc(A, D) :-
  pai_filho(A, X),
  ( X = D
  ; asc_desc(X, D)
  ).

asc_desc(A, D, P) :-
  pai_filho(A, X),
  ( X = D, P = []
  ; P = [X | P1], asc_desc(X, D, P1)
  ).
