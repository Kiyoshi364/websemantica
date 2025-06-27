rdf(iri('http://example.com/base/John'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')).
rdf(iri('http://example.com/base/John'), iri('http://xmlns.com/foaf/0.1/name'), literal('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString', @("John", "en"))).
rdf(iri('http://example.com/base/John'), iri('http://example.com/base/age'), literal('http://www.w3.org/2001/XMLSchema#integer', "32")).
rdf(iri('http://example.com/base/John'), iri('http://xmlns.com/foaf/0.1/phone'), literal('http://www.w3.org/2001/XMLSchema#string', "+123456")).
rdf(iri('http://example.com/base/John'), iri('http://example.com/base/gender'), iri('http://example.com/base/male')).
rdf(iri('http://example.com/base/Tim'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')).
rdf(iri('http://example.com/base/Tim'), iri('http://xmlns.com/foaf/0.1/name'), literal(iri('http://www.w3.org/2001/XMLSchema#string'), "Tim")).
rdf(iri('http://example.com/base/Tim'), iri('http://example.com/base/age'), literal('http://www.w3.org/2001/XMLSchema#integer', "20")).
rdf(iri('http://example.com/base/Tim'), iri('http://example.com/base/gender'), iri('http://example.com/base/male')).
rdf(iri('http://example.com/base/Tim'), iri('http://dbpedia.org/ontology/birthPlace'), iri('http://dbpedia.org/resource/Hawaii')).
rdf(iri('http://example.com/base/Jane'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), iri('http://xmlns.com/foaf/0.1/Person')).
rdf(iri('http://example.com/base/Jane'), iri('http://www.w3.org/2000/01/rdf-schema#label'), literal('http://www.w3.org/2001/XMLSchema#string', "Jane")).
rdf(iri('http://example.com/base/Jane'), iri('http://example.com/base/age'), literal('http://www.w3.org/2001/XMLSchema#integer', "23")).
rdf(iri('http://example.com/base/Jane'), iri('http://example.com/base/gender'), iri('http://example.com/base/female')).
rdf(iri('http://example.com/base/John'), iri('http://xmlns.com/foaf/0.1/knows'), iri('http://example.com/base/Tim')).
rdf(iri('http://example.com/base/Tim'), iri('http://xmlns.com/foaf/0.1/knows'), iri('http://example.com/base/John')).

:- use_module(library(lists), [memberchk/2]).

pp([
rdf - 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
rdfs - 'http://www.w3.org/2000/01/rdf-schema#',
xsd - 'http://www.w3.org/2001/XMLSchema#',
foaf - 'http://xmlns.com/foaf/0.1/',
(:) - 'http://example.com/base/',
dbr - 'http://dbpedia.org/resource/',
dbo - 'http://dbpedia.org/ontology/'
]).

prefix(P, L, iri(X)) :-
  pp(PP),
  memberchk(P-N, PP),
  atom_concat(N, L, X).
