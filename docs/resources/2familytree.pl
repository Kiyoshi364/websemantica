rdf(sergio, parent_child, milton).      rdf(lara, parent_child, milton).
rdf(milton, parent_child, george).      rdf(milton, parent_child, carmem).
rdf(helena, parent_child, george).      rdf(helena, parent_child, carmem).
rdf(george, parent_child, ana).         rdf(george, parent_child, andre).
rdf(mario, parent_child, maria).        rdf(dora, parent_child, maria).
rdf(maria, parent_child, ana).          rdf(maria, parent_child, andre).
rdf(carmem, parent_child, sara).        rdf(carmem, parent_child, ema).
rdf(alexandre, parent_child, sara).     rdf(alexandre, parent_child, ema).

rdf(sergio, isA, male).     rdf(milton, isA, male).     rdf(george, isA, male).
rdf(mario, isA, male).      rdf(alexandre, isA, male).  rdf(andre, isA, male).

rdf(lara, isA, female).     rdf(carmem, isA, female).   rdf(helena, isA, female).
rdf(dora, isA, female).     rdf(maria, isA, female).    rdf(ana, isA, female).
rdf(sara, isA, female).     rdf(ema, isA, female).

plus(Sub, Pred, Obj) :-
  rdf(Sub, Pred, X),
  ( X = Obj ; plus(X, Pred, Obj) ).

plus(Sub, Pred, Obj, P) :-
  rdf(Sub, Pred, X),
  ( P = [], X = Obj ; P = [X | P1], plus(X, Pred, Obj, P1) ).
