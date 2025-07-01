:- module(semweb_unord_lists, [
  empty_graph/1, is_graph/1,
  list_to_graph/2, graph_to_list/2,
  put_svo_graph/5, put_triple_graph/3,
  graph_svo/4, graph_triple/2,
  query_graph/2, graph_findall/4
]).

:- use_module(library(lists), [
  member/2, foldl/4
]).
:- use_module(library(reif), [
  dif/3, (',')/3, tfilter/3
]).

:- use_module('../semweb_error', [
  must_be_triple/1,
  must_be_subject/1, must_be_verb/1, must_be_object/1,
  triple_t/2
]).

%%%%% Graph Construction %%%%%

empty_graph([]).

is_graph([]).
is_graph([T | G]) :- triple_t(T, true), is_graph(G).

graph_to_list(G, G).
list_to_graph(L, G) :-
  empty_graph(G0),
  % NOTE: this is to check if the list has valid triples
  foldl(put_triple_graph, L, G0, G).

put_svo_graph(S, V, O, G0, G) :-
  must_be_subject(S),
  must_be_verb(V),
  must_be_object(O),
  put_triple_graph_(t(S, V, O), G0, G).

put_triple_graph(T, G0, G) :-
  must_be_triple(T),
  put_triple_graph_(T, G0, G).

put_triple_graph_(T, G0, [T | G0]).

%%%%% Graph Query %%%%%

graph_svo(G, S, V, O) :- graph_triple(G, t(S, V, O)).

graph_triple(G, T) :- member(T, G).

query_graph(( Qa , Qb ), G) :- query_graph(Qa, G), query_graph(Qb, G).
query_graph(( Qa ; Qb ), G) :-
  \+ subsumes_term(( _ -> _ ), Qa),
  ( query_graph(Qa, G) ; query_graph(Qb, G) ).
query_graph(( Qa0 -> Qa1 ; Qb ), G) :-
  ( query_graph(Qa0, G) -> query_graph(Qa1, G)
  ; query_graph(Qb, G)
  ).
query_graph({ Goal }, _) :- call(Goal).
query_graph(rdf(S, V, O), G) :- graph_svo(G, S, V, O).
query_graph(grdf(AG, S, V, O), _) :- graph_svo(AG, S, V, O).

graph_findall(G, X, Q, Xs) :- findall(X, query_graph(Q, G), Xs).
