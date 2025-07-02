:- module(test_ttl2pl, [
  run_tests/0, run_tests/1
]).

:- use_module('../parsers/turtle', [
  parse//2
]).

:- initialization(consult(ttl2pl)).

:- use_module(library(dcgs), [phrase/3]).
:- use_module(library(lists), [append/3, foldl/4]).
:- use_module(library(debug)).

:- use_module('../libtest').

join_partlist(Join, L, S0, S) :- append(L, [Join | S], S0).

nwdet_ok(_:T) :- nwdet(T).
:- discontiguous(nwdet/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BEGIN TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% META  TTL2PL %%%%%%%%%%%%%%%

meta_test_pl_output(In, Out) :-
  meta_test_pl_output(In, Out, XIn, CatOut, XOut),
  XIn == [],
  XOut == CatOut,
  !.

meta_test_pl_output(In, Out, XIn, CatOut, XOut) :-
  foldl(join_partlist('\n'), In, CatIn, []),
  foldl(join_partlist('\n'), Out, CatOut, []),
  phrase(parse(Ts, _), CatIn, XIn),
  phrase(foldl(user:triple, Ts), XOut, []).

meta_test_pl_output_(In, Out) :-
  meta_test_pl_output(In, Out, XIn, CatOut, XOut),
  writen(xin),
  $(XIn == []),
  writen(xout),
  $(XOut == CatOut),
  !.

%%%%%%%%%%%%%%% BEGIN TTL2PL %%%%%%%%%%%%%%%

test_iri :-
  In = [
    "<sub> <verb> <obj> ."
  ],
  Out = [
    "t(iri(sub), iri(verb), iri(obj))."
  ],
  meta_test_pl_output(In, Out),
true.

test_blank :-
  In = [
    "_:a <verb> [ <verb2> <obj2> ] ."
  ],
  Out = [
    "t(blank(unlabeled, 0), iri(verb2), iri(obj2)).",
    "t(blank(labeled, '_:a'), iri(verb), blank(unlabeled, 0))."
  ],
  meta_test_pl_output(In, Out),
true.

test_literal :-
  In = [
    "<sub> <verb> 1 .",
    "<sub2> <verb2> \"str\"@lang ."
  ],
  Out = [
    "t(iri(sub), iri(verb), literal(iri('http://www.w3.org/2001/XMLSchema#integer'), \"1\")).",
    "t(iri(sub2), iri(verb2), literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @(\"str\", \"lang\")))."
  ],
  meta_test_pl_output(In, Out),
true.

test_maria :-
  In = [
    "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .",
    "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .",
    "@prefix owl: <http://www.w3.org/2002/07/owl#> .",
    "@prefix foaf: <http://xmlns.com/foaf/0.1/> .",
    "@prefix dc: <http://purl.org/dc/elements/1.1/> .",
    "@prefix dbo: <http://dbpedia.org/ontology/> .",
    "@prefix dbr: <http://dbpedia.org/resource/> .",
    "",
    "@prefix : <http://example.org/> .",
    "",
    ":Maria",
    "  rdf:type foaf:Person ;",
    "  foaf:name \"Maria\" ;",
    "  dbo:birthPlace dbr:Brazil ;",
    "  dbo:birthDate \"1980-06-15-03\"^^xsd:date ;",
    "  foaf:knows :Joao",
    ".",
    "",
    ":Joao",
    "  rdf:type foaf:Person ;",
    "  foaf:name \"João\" ;",
    "  foaf:interest :MonaLisa",
    ".",
    "",
    ":MonaLisa",
    "  owl:sameAs dbr:Mona_Lisa ;",
    "  rdfs:label \"Mona Lisa\"@en ;",
    "  dc:creator dbr:Leonardo_da_Vinci ;",
    "  dbo:museum :Louvre",
    ".",
    "",
    ":Louvre",
    "  owl:sameAs dbr:Louvre ;",
    "  dbo:location dbr:France",
    ".",
    "",
    "[] foaf:knows _:a ."
  ],
  Out = [
    "t(iri('http://example.org/Maria'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')).",
    "t(iri('http://example.org/Maria'), iri('http://xmlns.com/foaf/0.1/name'), literal(iri('http://www.w3.org/2001/XMLSchema#string'), \"Maria\")).",
    "t(iri('http://example.org/Maria'), iri('http://dbpedia.org/ontology/birthPlace'), iri('http://dbpedia.org/resource/Brazil')).",
    "t(iri('http://example.org/Maria'), iri('http://dbpedia.org/ontology/birthDate'), literal(iri('http://www.w3.org/2001/XMLSchema#date'), \"1980-06-15-03\")).",
    "t(iri('http://example.org/Maria'), iri('http://xmlns.com/foaf/0.1/knows'), iri('http://example.org/Joao')).",
    "t(iri('http://example.org/Joao'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')).",
    "t(iri('http://example.org/Joao'), iri('http://xmlns.com/foaf/0.1/name'), literal(iri('http://www.w3.org/2001/XMLSchema#string'), \"João\")).",
    "t(iri('http://example.org/Joao'), iri('http://xmlns.com/foaf/0.1/interest'), iri('http://example.org/MonaLisa')).",
    "t(iri('http://example.org/MonaLisa'), iri('http://www.w3.org/2002/07/owl#sameAs'), iri('http://dbpedia.org/resource/Mona_Lisa')).",
    "t(iri('http://example.org/MonaLisa'), iri('http://www.w3.org/2000/01/rdf-schema#label'), literal(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), @(\"Mona Lisa\", \"en\"))).",
    "t(iri('http://example.org/MonaLisa'), iri('http://purl.org/dc/elements/1.1/creator'), iri('http://dbpedia.org/resource/Leonardo_da_Vinci')).",
    "t(iri('http://example.org/MonaLisa'), iri('http://dbpedia.org/ontology/museum'), iri('http://example.org/Louvre')).",
    "t(iri('http://example.org/Louvre'), iri('http://www.w3.org/2002/07/owl#sameAs'), iri('http://dbpedia.org/resource/Louvre')).",
    "t(iri('http://example.org/Louvre'), iri('http://dbpedia.org/ontology/location'), iri('http://dbpedia.org/resource/France')).",
    "t(blank(unlabeled, 0), iri('http://xmlns.com/foaf/0.1/knows'), blank(labeled, '_:a'))."
  ],
  meta_test_pl_output(In, Out),
true.

%%%%%%%%%%%%%%%  END  TTL2PL %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  END  TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_tests :- run_tests("test_").

run_tests(Prefix) :- run_tests(Prefix, test_ttl2pl).

writen(X) :- write(X), nl.
