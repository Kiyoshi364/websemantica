:- module(test_implementation, [
  run_tests/1, run_tests/2
]).

:- use_module(library(reif), [tpartition/4]).
:- use_module(library(lists), [foldl/4]).
:- meta_predicate(foldl(5, ?, ?, ?, ?, ?)).
foldl(_, [], [], [], A, A).
foldl(G_5, [X | Xs], [Y | Ys], [Z | Zs], A0, A) :-
  call(G_5, X, Y, Z, A0, A1),
  foldl(G_5, Xs, Ys, Zs, A1, A).

:- use_module(library(debug)).

:- use_module('../libtest', [run_tests/3]).

load_semweb_module(M) :- use_module(M, [
  empty_graph/1, is_graph/1,
  list_to_graph/2, graph_to_list/2,
  put_spo_graph/5, put_triple_graph/3,
  del_spo_graph/5, del_triple_graph/3,
  graph_spo/4, graph_triple/2
]).

nwdet_ok(_:T) :- nwdet(T).
:- discontiguous(nwdet/1).

cassert(G) :-
  ( \+ call(G) -> throw(error(c_assertion_failed(G)))
  ; call(G)
  ).

cassert_once(G) :-
  ( call(G) -> true
  ; throw(error(c_assertion_failed(G)))
  ).

database([
  t(iri('http://example.com/base/John'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')),
  t(iri('http://example.com/base/John'), iri('http://xmlns.com/foaf/0.1/name'), literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @("John", "en"))),
  t(iri('http://example.com/base/John'), iri('http://example.com/base/age'), literal(iri('http://www.w3.org/2001/XMLSchema#integer'), "32")),
  t(iri('http://example.com/base/John'), iri('http://xmlns.com/foaf/0.1/phone'), literal(iri('http://www.w3.org/2001/XMLSchema#string'), "+123456")),
  t(iri('http://example.com/base/John'), iri('http://example.com/base/gender'), iri('http://example.com/base/male')),
  t(iri('http://example.com/base/Tim'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')),
  t(iri('http://example.com/base/Tim'), iri('http://xmlns.com/foaf/0.1/name'), literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")),
  t(iri('http://example.com/base/Tim'), iri('http://example.com/base/age'), literal(iri('http://www.w3.org/2001/XMLSchema#integer'), "20")),
  t(iri('http://example.com/base/Tim'), iri('http://example.com/base/gender'), iri('http://example.com/base/male')),
  t(iri('http://example.com/base/Tim'), iri('http://dbpedia.org/ontology/birthPlace'), iri('http://dbpedia.org/resource/Hawaii')),
  t(iri('http://example.com/base/Jane'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')),
  t(iri('http://example.com/base/Jane'), iri('http://www.w3.org/2000/01/rdf-schema#label'), literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Jane")),
  t(iri('http://example.com/base/Jane'), iri('http://example.com/base/age'), literal(iri('http://www.w3.org/2001/XMLSchema#integer'), "23")),
  t(iri('http://example.com/base/Jane'), iri('http://example.com/base/gender'), iri('http://example.com/base/female')),
  t(iri('http://example.com/base/John'), iri('http://xmlns.com/foaf/0.1/knows'), iri('http://example.com/base/Tim')),
  t(iri('http://example.com/base/Tim'), iri('http://xmlns.com/foaf/0.1/knows'), iri('http://example.com/base/John'))
]).

alt_database([
  t(iri('http://example.com/base/Jane'), iri('http://dbpedia.org/ontology/birthPlace'), iri('http://dbpedia.org/resource/Hampshire')),
  t(iri('http://example.com/base/Jane'), iri('http://dbpedia.org/ontology/birthDate'), literal(iri('http://www.w3.org/2001/XMLSchema#date'), "2000-07-07"))
]).

triples_spos([], [], [], []).
triples_spos([t(S, P, O) | Ts], [S | Ss], [P | Ps], [O | Os]) :- triples_spos(Ts, Ss, Ps, Os).

tpartition_sort(Ts, Sorteds) :- tpartition_sort_(Ts, Sorteds, []).

tpartition_sort_([], Xs, Xs).
tpartition_sort_([T | Ts], Xs0, Xs) :-
  tpartition(lessthan_t(T), Ts, Rs, Ls),
  tpartition_sort_(Ls, Xs0, [T | Xs1]),
  tpartition_sort_(Rs, Xs1, Xs).

lessthan_t(A, B, T) :-
  ( var(A) -> throw(error(instantiation_error, 1:lessthan_t(A, B, T)))
  ; var(B) -> throw(error(instantiation_error, 2:lessthan_t(A, B, T)))
  ; A @< B -> T = true
  ; T = false
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BEGIN TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% META  Construction %%%%%%%%%%%%%%%

meta_test_construction(Lib, ExpList, Graph) :-
  meta_test_construction(Lib, ExpList, Graph, XExpList, XList),
  cassert_once(XExpList == ExpList),
  XList == ExpList.

meta_test_construction(Lib, ExpList, Graph, XExpList, XList) :-
  tpartition_sort(ExpList, XExpList),
  Lib:graph_to_list(Graph, XList0),
  tpartition_sort(XList0, XList).

meta_test_construction_(Lib, ExpList, Graph) :-
  meta_test_construction(Lib, ExpList, Graph, XExpList, XList),
  $cassert_once(XExpList == ExpList),
  $(XList == ExpList).

%%%%%%%%%%%%%%% BEGIN Construction %%%%%%%%%%%%%%%

test_construction_empty_graph(Lib) :-
  Exp = [],
  Lib:empty_graph(G),
  meta_test_construction(Lib, Exp, G),
true.

test_construction_list_to_graph(Lib) :-
  database(Ts),
  tpartition_sort(Ts, Exp),
  Lib:list_to_graph(Ts, G),
  meta_test_construction(Lib, Exp, G),
true.

test_construction_is_graph_empty(Lib) :-
  Lib:empty_graph(G),
  Lib:is_graph(G),
true.

test_construction_is_graph_database(Lib) :-
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Lib:is_graph(G),
true.

test_construction_put_spo_graph(Lib) :-
  database(Ts),
  tpartition_sort(Ts, Exp),
  triples_spos(Ts, Ss, Ps, Os),
  Lib:empty_graph(G0),
  foldl(Lib:put_spo_graph, Ss, Ps, Os, G0, G),
  meta_test_construction(Lib, Exp, G),
true.

test_construction_put_triple_graph(Lib) :-
  database(Ts),
  tpartition_sort(Ts, Exp),
  Lib:empty_graph(G0),
  foldl(Lib:put_triple_graph, Ts, G0, G),
  meta_test_construction(Lib, Exp, G),
true.

test_construction_del_spo_empty(Lib) :-
  Lib:empty_graph(Exp),
  Lib:empty_graph(G0),
  S = iri(subject),
  P = iri(predicate),
  O = iri(object),
  Lib:del_spo_graph(S, P, O, G0, G),
  meta_test_construction(Lib, Exp, G),
true.

test_construction_del_triple_empty(Lib) :-
  Lib:empty_graph(Exp),
  Lib:empty_graph(G0),
  T = t(iri(subject), iri(predicate), iri(object)),
  Lib:del_triple_graph(T, G0, G),
  meta_test_construction(Lib, Exp, G),
true.

test_construction_del_spo_graph(Lib) :-
  database(Ts),
  Lib:empty_graph(Exp),
  Lib:list_to_graph(Ts, G0),
  triples_spos(Ts, Ss, Ps, Os),
  foldl(Lib:del_spo_graph, Ss, Ps, Os, G0, G),
  meta_test_construction(Lib, Exp, G),
true.

test_construction_del_triple_graph(Lib) :-
  database(Ts),
  Lib:empty_graph(Exp),
  Lib:list_to_graph(Ts, G0),
  foldl(Lib:del_triple_graph, Ts, G0, G),
  meta_test_construction(Lib, Exp, G),
true.

%%%%%%%%%%%%%%%  END  Construction %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% META  Query Inline %%%%%%%%%%%%%%%

meta_test_query_inline(Lib, ExpList, Select, Query) :-
  meta_test_query_inline(Lib, ExpList, Select, Query, XExpList, XList),
  cassert_once(XExpList == ExpList),
  XList == ExpList.

meta_test_query_inline(Lib, ExpList, Select, Query, XExpList, XList) :-
  Lib = Lib,
  tpartition_sort(ExpList, XExpList),
  findall(Select, Query, XList0),
  tpartition_sort(XList0, XList).

meta_test_query_inline_(Lib, ExpList, Select, Query) :-
  meta_test_query_inline(Lib, ExpList, Select, Query, XExpList, XList),
  $cassert_once(XExpList == ExpList),
  $(XList == ExpList).

%%%%%%%%%%%%%%% BEGIN Query Inline %%%%%%%%%%%%%%%

test_query_inline_spo_empty(Lib) :-
  Exp = [],
  Lib:empty_graph(G),
  Select = Select,
  Query = Lib:graph_spo(G, _, _, _),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_triple_empty(Lib) :-
  Exp = [],
  Lib:empty_graph(G),
  Select = Select,
  Query = Lib:graph_triple(G, _),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_spo_tim_friend_name(Lib) :-
  Tim = iri('http://example.com/base/Tim'),
  FoafKnows = iri('http://xmlns.com/foaf/0.1/knows'),
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  Exp = [
    iri('http://example.com/base/John')-literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @("John", "en"))
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Friend-Name,
  Query = (
    Lib:graph_spo(G, Tim, FoafKnows, Friend),
    Lib:graph_spo(G, Friend, FoafName, Name)
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_triple_tim_friend_name(Lib) :-
  Tim = iri('http://example.com/base/Tim'),
  FoafKnows = iri('http://xmlns.com/foaf/0.1/knows'),
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  Exp = [
    iri('http://example.com/base/John')-literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @("John", "en"))
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Friend-Name,
  Query = (
    Lib:graph_triple(G, t(Tim, FoafKnows, Friend)),
    Lib:graph_triple(G, t(Friend, FoafName, Name))
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_spo_tim_friend_and_name(Lib) :-
  FoafKnows = iri('http://xmlns.com/foaf/0.1/knows'),
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  Exp = [
    iri('http://example.com/base/John')-literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @("John", "en")),
    iri('http://example.com/base/John')-literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim"),
    iri('http://example.com/base/Tim')-literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @("John", "en")),
    iri('http://example.com/base/Tim')-literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Person-FName,
  Query = (
    Lib:graph_spo(G, Person, FoafKnows, _),
    Lib:graph_spo(G, _, FoafName, FName)
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_triple_tim_friend_and_name(Lib) :-
  FoafKnows = iri('http://xmlns.com/foaf/0.1/knows'),
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  Exp = [
    iri('http://example.com/base/John')-literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @("John", "en")),
    iri('http://example.com/base/John')-literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim"),
    iri('http://example.com/base/Tim')-literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @("John", "en")),
    iri('http://example.com/base/Tim')-literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Person-FName,
  Query = (
    Lib:graph_triple(G, t(Person, FoafKnows, _)),
    Lib:graph_triple(G, t(_, FoafName, FName))
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_spo_tim_name_optphone(Lib) :-
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  FoafPhone = iri('http://xmlns.com/foaf/0.1/phone'),
  Exp = [
    literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @("John", "en"))-literal(iri('http://www.w3.org/2001/XMLSchema#string'), "+123456"),
    literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")-null
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Name-Phone,
  Query = (
    Lib:graph_spo(G, Person, FoafName, Name),
    ( \+ Lib:graph_spo(G, Person, FoafPhone, Phone) -> Phone = null
    ; Lib:graph_spo(G, Person, FoafPhone, Phone)
    )
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_triple_tim_name_optphone(Lib) :-
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  FoafPhone = iri('http://xmlns.com/foaf/0.1/phone'),
  Exp = [
    literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @("John", "en"))-literal(iri('http://www.w3.org/2001/XMLSchema#string'), "+123456"),
    literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")-null
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Name-Phone,
  Query = (
    Lib:graph_triple(G, t(Person, FoafName, Name)),
    ( \+ Lib:graph_triple(G, t(Person, FoafPhone, Phone)) -> Phone = null
    ; Lib:graph_triple(G, t(Person, FoafPhone, Phone))
    )
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_spo_tim_nameorlabel(Lib) :-
  Type = iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  FoafPerson = iri('http://xmlns.com/foaf/0.1/Person'),
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  Label = iri('http://www.w3.org/2000/01/rdf-schema#label'),
  Exp = [
    literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @("John", "en")),
    literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Jane"),
    literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Name,
  Query = (
    Lib:graph_spo(G, Person, Type, FoafPerson),
    ( Lib:graph_spo(G, Person, FoafName, Name)
    ; Lib:graph_spo(G, Person, Label, Name)
    )
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_triple_tim_nameorlabel(Lib) :-
  Type = iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  FoafPerson = iri('http://xmlns.com/foaf/0.1/Person'),
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  Label = iri('http://www.w3.org/2000/01/rdf-schema#label'),
  Exp = [
    literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @("John", "en")),
    literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Jane"),
    literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Name,
  Query = (
    Lib:graph_triple(G, t(Person, Type, FoafPerson)),
    ( Lib:graph_triple(G, t(Person, FoafName, Name))
    ; Lib:graph_triple(G, t(Person, Label, Name))
    )
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_spo_tim_describe_tim(Lib) :-
  Tim = iri('http://example.com/base/Tim'),
  Exp = [
    t(iri('http://example.com/base/John'), iri('http://xmlns.com/foaf/0.1/knows'), iri('http://example.com/base/Tim')),
    t(iri('http://example.com/base/Tim'), iri('http://dbpedia.org/ontology/birthPlace'), iri('http://dbpedia.org/resource/Hawaii')),
    t(iri('http://example.com/base/Tim'), iri('http://example.com/base/age'), literal(iri('http://www.w3.org/2001/XMLSchema#integer'), "20")),
    t(iri('http://example.com/base/Tim'), iri('http://example.com/base/gender'), iri('http://example.com/base/male')),
    t(iri('http://example.com/base/Tim'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')),
    t(iri('http://example.com/base/Tim'), iri('http://xmlns.com/foaf/0.1/knows'), iri('http://example.com/base/John')),
    t(iri('http://example.com/base/Tim'), iri('http://xmlns.com/foaf/0.1/name'), literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim"))
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = t(S, P, O),
  Query = (
    Lib:graph_spo(G, S, P, O),
    ( S = Tim ; O = Tim )
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_triple_tim_describe_tim(Lib) :-
  Tim = iri('http://example.com/base/Tim'),
  Exp = [
    t(iri('http://example.com/base/John'), iri('http://xmlns.com/foaf/0.1/knows'), iri('http://example.com/base/Tim')),
    t(iri('http://example.com/base/Tim'), iri('http://dbpedia.org/ontology/birthPlace'), iri('http://dbpedia.org/resource/Hawaii')),
    t(iri('http://example.com/base/Tim'), iri('http://example.com/base/age'), literal(iri('http://www.w3.org/2001/XMLSchema#integer'), "20")),
    t(iri('http://example.com/base/Tim'), iri('http://example.com/base/gender'), iri('http://example.com/base/male')),
    t(iri('http://example.com/base/Tim'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')),
    t(iri('http://example.com/base/Tim'), iri('http://xmlns.com/foaf/0.1/knows'), iri('http://example.com/base/John')),
    t(iri('http://example.com/base/Tim'), iri('http://xmlns.com/foaf/0.1/name'), literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim"))
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = t(S, P, O),
  Query = (
    Lib:graph_triple(G, t(S, P, O)),
    ( S = Tim ; O = Tim )
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_spo_tim_name_phone_not_bound(Lib) :-
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  FoafPhone = iri('http://xmlns.com/foaf/0.1/phone'),
  Exp = [
    literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Name,
  Query = (
    Lib:graph_spo(G, Person, FoafName, Name),
    ( \+ Lib:graph_spo(G, Person, FoafPhone, Phone) -> Phone = null
    ; Lib:graph_spo(G, Person, FoafPhone, Phone)
    ),
    Phone = null
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_triple_tim_name_phone_not_bound(Lib) :-
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  FoafPhone = iri('http://xmlns.com/foaf/0.1/phone'),
  Exp = [
    literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Name,
  Query = (
    Lib:graph_triple(G, t(Person, FoafName, Name)),
    ( \+ Lib:graph_triple(G, t(Person, FoafPhone, Phone)) -> Phone = null
    ; Lib:graph_triple(G, t(Person, FoafPhone, Phone))
    ),
    Phone = null
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_spo_tim_name_without_phone_not_exists(Lib) :-
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  FoafPhone = iri('http://xmlns.com/foaf/0.1/phone'),
  Exp = [
    literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Name,
  Query = (
    Lib:graph_spo(G, Person, FoafName, Name),
    \+ Lib:graph_spo(G, Person, FoafPhone, _)
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_triple_tim_name_without_phone_not_exists(Lib) :-
  FoafName = iri('http://xmlns.com/foaf/0.1/name'),
  FoafPhone = iri('http://xmlns.com/foaf/0.1/phone'),
  Exp = [
    literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")
  ],
  database(Ts),
  Lib:list_to_graph(Ts, G),
  Select = Name,
  Query = (
    Lib:graph_triple(G, t(Person, FoafName, Name)),
    \+ Lib:graph_triple(G, t(Person, FoafPhone, _))
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_spo_tim_jane_describe_jane(Lib) :-
  Jane = iri('http://example.com/base/Jane'),
  Exp = [
    t(iri('http://example.com/base/Jane'), iri('http://dbpedia.org/ontology/birthDate'), literal(iri('http://www.w3.org/2001/XMLSchema#date'), "2000-07-07")),
    t(iri('http://example.com/base/Jane'), iri('http://dbpedia.org/ontology/birthPlace'), iri('http://dbpedia.org/resource/Hampshire')),
    t(iri('http://example.com/base/Jane'), iri('http://example.com/base/age'), literal(iri('http://www.w3.org/2001/XMLSchema#integer'), "23")),
    t(iri('http://example.com/base/Jane'), iri('http://example.com/base/gender'), iri('http://example.com/base/female')),
    t(iri('http://example.com/base/Jane'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')),
    t(iri('http://example.com/base/Jane'), iri('http://www.w3.org/2000/01/rdf-schema#label'), literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Jane"))
  ],
  database(Ts),
  alt_database(ATs),
  Lib:list_to_graph(Ts, G),
  Lib:list_to_graph(ATs, AG),
  Select = t(S, P, O),
  Query = (
    ( Lib:graph_spo(G, S, P, O)
    ; Lib:graph_spo(AG, S, P, O)
    ),
    ( S = Jane ; O = Jane )
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

test_query_inline_triple_tim_jane_describe_jane(Lib) :-
  Jane = iri('http://example.com/base/Jane'),
  Exp = [
    t(iri('http://example.com/base/Jane'), iri('http://dbpedia.org/ontology/birthDate'), literal(iri('http://www.w3.org/2001/XMLSchema#date'), "2000-07-07")),
    t(iri('http://example.com/base/Jane'), iri('http://dbpedia.org/ontology/birthPlace'), iri('http://dbpedia.org/resource/Hampshire')),
    t(iri('http://example.com/base/Jane'), iri('http://example.com/base/age'), literal(iri('http://www.w3.org/2001/XMLSchema#integer'), "23")),
    t(iri('http://example.com/base/Jane'), iri('http://example.com/base/gender'), iri('http://example.com/base/female')),
    t(iri('http://example.com/base/Jane'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')),
    t(iri('http://example.com/base/Jane'), iri('http://www.w3.org/2000/01/rdf-schema#label'), literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Jane"))
  ],
  database(Ts),
  alt_database(ATs),
  Lib:list_to_graph(Ts, G),
  Lib:list_to_graph(ATs, AG),
  Select = t(S, P, O),
  Query = (
    ( Lib:graph_triple(G, t(S, P, O))
    ; Lib:graph_triple(AG, t(S, P, O))
    ),
    ( S = Jane ; O = Jane )
  ),
  meta_test_query_inline(Lib, Exp, Select, Query),
true.

%%%%%%%%%%%%%%%  END  Query Inline %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  END  TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_tests(Lib) :- run_tests(Lib, "test_").
run_tests(Lib, Prefix) :- load_semweb_module(Lib), run_tests(Prefix, test_implementation, Lib).

writen(X) :- write(X), nl.
