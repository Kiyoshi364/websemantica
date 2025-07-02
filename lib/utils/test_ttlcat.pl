:- module(test_ttlcat, [
  run_tests/0, run_tests/1
]).

:- use_module('../parsers/turtle', [
  parse//2
]).

:- initialization(consult(ttlcat)).

:- use_module(library(dcgs), [phrase/3]).
:- use_module(library(lists), [append/3, foldl/4]).
:- use_module(library(debug)).

:- use_module('../libtest').

join_partlist(Join, L, S0, S) :- append(L, [Join | S], S0).

nwdet_ok(_:T) :- nwdet(T).
:- discontiguous(nwdet/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BEGIN TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% META  TTLCAT %%%%%%%%%%%%%%%

meta_test_cat_output(In, Out) :-
  meta_test_cat_output(In, Out, XIn, CatOut, XOut),
  XIn == [],
  XOut == CatOut,
  !.

meta_test_cat_output(In, Out, XIn, CatOut, XOut) :-
  foldl(join_partlist('\n'), In, CatIn, []),
  foldl(join_partlist('\n'), Out, CatOut, []),
  phrase(parse(Ts, _), CatIn, XIn),
  phrase(foldl(user:triple, Ts), XOut, []).

meta_test_cat_output_(In, Out) :-
  meta_test_cat_output(In, Out, XIn, CatOut, XOut),
  writen(xin),
  $(XIn == []),
  writen(xout),
  $(XOut == CatOut),
  !.

%%%%%%%%%%%%%%% BEGIN TTLCAT %%%%%%%%%%%%%%%

test_iri :-
  In = [
    "<sub> <verb> <obj> ."
  ],
  Out = [
    "<sub> <verb> <obj>."
  ],
  meta_test_cat_output(In, Out),
true.

test_blank :-
  In = [
    "_:a <verb> [ <verb2> <obj2> ] ."
  ],
  Out = [
    "_:0 <verb2> <obj2>.",
    "_:a <verb> _:0."
  ],
  meta_test_cat_output(In, Out),
true.

test_literal :-
  In = [
    "<sub> <verb> 1 .",
    "<sub2> <verb2> \"str\"@lang ."
  ],
  Out = [
    "<sub> <verb> \"1\"^^<http://www.w3.org/2001/XMLSchema#integer>.",
    "<sub2> <verb2> \"str\"@lang."
  ],
  meta_test_cat_output(In, Out),
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
    "<http://example.org/Maria> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person>.",
    "<http://example.org/Maria> <http://xmlns.com/foaf/0.1/name> \"Maria\"^^<http://www.w3.org/2001/XMLSchema#string>.",
    "<http://example.org/Maria> <http://dbpedia.org/ontology/birthPlace> <http://dbpedia.org/resource/Brazil>.",
    "<http://example.org/Maria> <http://dbpedia.org/ontology/birthDate> \"1980-06-15-03\"^^<http://www.w3.org/2001/XMLSchema#date>.",
    "<http://example.org/Maria> <http://xmlns.com/foaf/0.1/knows> <http://example.org/Joao>.",
    "<http://example.org/Joao> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person>.",
    "<http://example.org/Joao> <http://xmlns.com/foaf/0.1/name> \"João\"^^<http://www.w3.org/2001/XMLSchema#string>.",
    "<http://example.org/Joao> <http://xmlns.com/foaf/0.1/interest> <http://example.org/MonaLisa>.",
    "<http://example.org/MonaLisa> <http://www.w3.org/2002/07/owl#sameAs> <http://dbpedia.org/resource/Mona_Lisa>.",
    "<http://example.org/MonaLisa> <http://www.w3.org/2000/01/rdf-schema#label> \"Mona Lisa\"@en.",
    "<http://example.org/MonaLisa> <http://purl.org/dc/elements/1.1/creator> <http://dbpedia.org/resource/Leonardo_da_Vinci>.",
    "<http://example.org/MonaLisa> <http://dbpedia.org/ontology/museum> <http://example.org/Louvre>.",
    "<http://example.org/Louvre> <http://www.w3.org/2002/07/owl#sameAs> <http://dbpedia.org/resource/Louvre>.",
    "<http://example.org/Louvre> <http://dbpedia.org/ontology/location> <http://dbpedia.org/resource/France>.",
    "_:0 <http://xmlns.com/foaf/0.1/knows> _:a."
  ],
  meta_test_cat_output(In, Out),
true.

%%%%%%%%%%%%%%%  END  TTLCAT %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  END  TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_tests :- run_tests("test_").

run_tests(Prefix) :- run_tests(Prefix, test_ttlcat).

writen(X) :- write(X), nl.
