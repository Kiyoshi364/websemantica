:- module(semweb_unord_lists, [
  empty_graph/1, is_graph/1,
  list_to_graph/2, graph_to_list/2,
  put_svo_graph/5, put_triple_graph/3,
  del_svo_graph/5, del_triple_graph/3,
  graph_svo/4, graph_triple/2,
  graph_prefixes_svo/5, graph_prefixes_triple/3
]).

:- use_module(library(lists), [
  member/2, foldl/4
]).
:- use_module(library(reif), [
  dif/3, (',')/3, tfilter/3
]).

:- use_module(semweb_error, [
  must_be_subject/1, must_be_verb/1, must_be_object/1
]).

%%%%% Graph Construction %%%%%

empty_graph([]).

is_graph([]).
is_graph([T | G]) :- triple_t(T, true), is_graph(G).

graph_to_list(G, G).
list_to_graph(L, G) :-
  empty_graph(G0),
  % NOTE: this is to check if the list has valid triples
  foldl(triple_graph_with, L, G0, G).

put_svo_graph(S, V, O, G0, G) :-
  must_be_subject(S),
  must_be_verb(V),
  must_be_object(O),
  put_triple_graph_(t(S, V, O), G0, G).

put_triple_graph(T, G0, G) :-
  must_be_triple(T),
  triple_graph_with_(T, G0, G).

put_triple_graph_(T, G0, [T | G0]).

del_svo_graph(S, V, O, G, G0) :-
  del_triple_graph_(t(S, V, O), G, G0).

del_triple_graph(T, G, G0) :-
  must_be_triple(T),
  del_triple_graph_(T, G, G0).

del_triple_graph_(T, G, G0) :-
  tfilter(dif(T), G, G0).

%%%%% Graph Query %%%%%

graph_svo(G, S, V, O) :- graph_triple(G, t(S, V, O)).

graph_triple(G, T) :- member(T, G).

graph_prefixes_svo(G, PP, S, V, O) :-
  strip_prefix(PP, S, S0),
  strip_prefix(PP, V, V0),
  strip_prefix(PP, O, O0),
  graph_triple(G, t(S0, V0, O0)),
  unstrip_prefix(PP, S, S0),
  unstrip_prefix(PP, V, V0),
  unstrip_prefix(PP, O, O0),

graph_prefixes_triple(G, PP, t(S, V, O)) :- graph_prefixes_svo(G, PP, S, V, O).
