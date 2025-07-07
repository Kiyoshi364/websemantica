property_path(Sub, PropPath, Obj) :- property_path(Sub, PropPath, Obj, _).

property_path(Sub, PropPath, Obj, P) :- pp_(PropPath, Sub, Obj, P, [Obj]).

% NOTE: assuming that the base case is tagged with an `iri`
pp_(iri(Pred), Sub, Obj, [Sub | P], P) :- rdf(Sub, Pred, Obj).
pp_(^(PP), Sub, Obj, P0, P) :- pp_(PP, Obj, Sub, P0, P).
pp_(/(PP1, PP2), Sub, Obj, P0 P) :-
  pp_(PP1, Sub, X, P0, P1),
  pp_(PP2, X, Obj, P1, P).
pp_(|(PP1, PP2), Sub, Obj, P0, P) :-
  ( pp_(PP1, Sub, Obj, P0, P)
  ; pp_(PP2, Sub, Obj, P0, P)
  ).
pp_(*(PP), Sub, Obj, P0, P) :-
  % NOTE: This is to avoid creating and destroying new functors
  pp_star(PP, Sub, Obj, P0, P).
pp_(+(PP), Sub, Obj, P0, P) :-
  % NOTE: This is to avoid creating and destroying new functors
  pp_plus(PP, Sub, Obj, P0, P).
pp_(?(PP), Sub, Obj, P0, P) :-
  ( Sub = Obj, P = P0
  ; pp_(PP, Sub, Obj, P0, P)
  ).
pp_(!(PPs), Sub, Obj, P0, P) :-
  throw(error(representation_error(property_path), [culprit- !(PPs)])).

pp_star(PP, Sub, Obj, P0, P) :-
  ( Sub = Obj, P = P0
  ; pp_plus(PP, Sub, Obj, P0, P)
  ).

pp_plus(PP, Sub, Obj, P0, P) :-
  pp_(PP, Sub, X, P0, P1),
  pp_star(PP, Sub, Obj, P1, P).
