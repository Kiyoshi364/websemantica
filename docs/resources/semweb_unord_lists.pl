:- module(semweb_unord_lists, [
  empty_graph/1, is_graph/1,
  list_to_graph/2, graph_to_list/2,
  put_spo_graph/5, put_triple_graph/3,
  del_spo_graph/5, del_triple_graph/3,
  graph_spo/4, graph_triple/2
]).

:- use_module(library(lists), [member/2, foldl/4]).
:- use_module(library(reif), [if_/3, (=)/3, memberd_t/3, tfilter/3]).

%%%%% Graph Construction %%%%%

empty_graph([]).

is_graph([]).
is_graph([T | G]) :-
  % type(triple, T),
  is_graph(G).

list_to_graph(L, G) :-
  empty_graph(G0),
  foldl(put_triple_graph, L, G0, G).

graph_to_list(G, G).

put_spo_graph(S, P, O, G0, G) :-
  % must_be(subject, S), must_be(predicate, P), must_be(object, O),
  put_triple_graph_(t(S, P, O), G0, G).

put_triple_graph(T, G0, G) :-
  % must_be(triple, T),
  put_triple_graph_(T, G0, G).

put_triple_graph_(T, G0, G) :-
  if_(memberd_t(T, G0), G = G0, G = [T | G0]).

del_spo_graph(S, P, O, G0, G) :-
  % must_be(subject, S), must_be(predicate, P), must_be(object, O),
  del_triple_graph_(t(S, P, O), G0, G).

del_triple_graph(T, G0, G) :-
  % must_be(triple, T),
  del_triple_graph_(T, G0, G).

del_triple_graph_(T, G0, G) :- tfilter_t(=(T), G0, _, G).

%%%%% Graph Querying %%%%%

graph_spo(G, S, P, O) :- graph_triple(G, t(S, P, O)).

graph_triple(G, T) :- member(T, G).
