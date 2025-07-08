#import "fine-lncs/lib.typ": lncs, institute, author;
#import "defs.typ": *;

#let inst_ic = institute(
  "Instituto de Computação",
  addr: "Universidade Federal do Rio de Janeiro, Brasil",
  url: "https://ufrj.br/en/",
);

#let abstract = [
  Semantic Web attempts to
  bring machine-readable information
  to the web
  by using frameworks such as
  Resource Description Framework (RDF).
  In RDF,
  the resources (data) are represented as graph nodes
  and the relations between these resources
  are represented as a collection of directed graph edges
  with the same label.
  SPARQL is
  the language commonly used for quering RDF databases
  (akin to SQL for relational databases).
  Prolog is a logic programming language
  suited for declarative and symbolic programming
  which we stablish some facts and rules (a knowledge database)
  and then
  we ask questions to the Prolog system about the defined knowledge
  to find out on which conditions the question is true.
  We can represent
  a RDF database and SPARQL queries
  using Prolog's facts and queries,
  respectively.
  In this report,
  we propose
  an implementation-independent Prolog interface
  for reasoning about semantic web databases
  and provide a referencence implementation
  for the this interface.
];

#let keywords = (
  "Semantic Web",
  "Resource Description Framework",
  "Prolog",
  "Logic Programming",
  "Triple Store",
  "SPARQL",
);

#show: lncs.with(
  title: "Towards an Independent Interface for Semantic Web Reasoning in Prolog",
  authors: (
    author("Daniel Kiyoshi Hashimoto",
      oicd: "0000-0002-3113-4488",
      insts: (inst_ic),
    ),
  ),
  abstract: abstract,
  keywords: keywords,
  bibliography: bibliography("refs.bib"),
);

#set document(
  keywords: keywords,
  date: datetime(year: 2025, month: 07, day: 18),
);
#set raw(syntaxes: syntaxes, block: true);
#show raw.where(block: true): show-raw-block;

= Introduction

The Semantic Web movement
is a modern attempt to
bring machine-readable data to the web.
The Resource Description Framework (RDF)#footnote(links.rdf-primer);
describes a way to represent
machine-readable information and knowledge
via a directed graph with labeled edges.
In this framework,
an edge
from node $S$
to node $O$
with label $P$
stands for an assertion
that the predicate $P$
holds for
the resources $S$ and $O$.
Such graph is commonly stored
in triple databases.
Ali, et al.~#cite(<triplestoresurvey>)
provide a survey
with implementation details
for efficient storage and query
for such graphs.

On the other side,
we have the Prolog,
a logic programing language
where information and knowledge
is naturally represented
with predicates
between symbols.
The semantics of Prolog programs
suit well with
the graph representation of
RDF databases.
We already have a library for
building semantic web applications
in Prolog: Cliopatria~#cite(<cliopatria>).
However,
Cliopatria is a SWI-Prolog library
with a C implementation
which makes it difficult to port
to others Prolog implementations.

This work ...

= Prolog and RDF

Before we describe the interface,
we give a brief introduction to
Prolog, RDF and SPARQL,
and how RDF databases
may be represented and queried
in Prolog.
For that,
we will provide an prolog example
then show a translation of this example
to RDF and SPARQL queries
and then back to Prolog.

== Prolog Language

Prolog programming
consists of describing what is true
by asserting truth facts and
rules for deriving new truths
(the program),
and then
ask true of false questions to the system
(the query),
which will try to simplify them
to some condition which makes the question true.
For further explanation,
consider the @prog:familytree-pl.
Facts and rules are terminated with a period/dot (```pl .```).
For instance,
```pl parent_child(sergio, milton).```
is a fact
and
```pl asc_desc(A, D) :- parent_child(A, X), ( X = D ; asc_desc(X, D) ).```
is a rule.

@prog:familytree-pl describes 3 predicates with facts
(```pl parent_child/2```,
```pl male/1```, and
```pl female/1```)
and 2 predicates with rules
(```pl asc_desc/2```, and
```pl asc_desc/3```).
The number after ```pl /```
indicates the arity (number of arguments) of the predicate
which is used to differentiate predicates.

#codefig(
  caption: [Family Tree in Prolog],
)[#{
  raw(lang: "pl", read(resource("0familytree.pl")));
}] <prog:familytree-pl>

The predicate ```pl parent_child(P, C)```
indicates that ```pl P```
is the parent of  ```pl C```
(which is a child of  ```pl P```).
The names
which start with an uppercase letter
are variables
and
the lowercase names appearing
inside the parenthesis
(```pl sergio```,
```pl milton```,
```pl lara```,
...)
are atoms:
named constants
representing distict individuals.
The predicate ```pl male(P)```
indicates that ```pl P``` is male,
likewise,
the predicate ```pl female(P)```
indicates that ```pl P``` is female.

Before looking at the rules of @prog:familytree-pl
we will take a look at queries
using the facts from this program.
In prolog,
we can make fully instantiated queries
(without any variables),
such as
```pl ?- male(helena).```,
and
```pl ?- parent_child(carmem, ema), female(ema).```.
These queries act as yes or no questions
to the prolog system.
The previous queries mean:
"is Helena male?",
and
"is Carmem parent of Ema and Ema female?".

We can also make
partially instantiated queries
to prolog,
such as
```pl ?- male(X).```,
and
```pl ?- parent_child(carmem, X), female(X).```.
In these queries,
the prolog system will attempt
to find a substitution of the variables
to make the query true.
These example queries mean,
respectively:
"which Xs are male?",
and
"which Xs are child of Carmem and are female?"
(alternatively, "which Xs are the daughters of Carmem?").

We show the answers to
both fully and partially instantiated
query examples,
when asked to Scryer Prolog,
in @repl:familytree.
Prolog uses
```pl ,``` and ```pl ;```
as the logical connectives "and" and "or".
In the previous examples,
we have queries using ```pl ,```
and answers using ```pl ;```.

Coming back to @prog:familytree-pl.
We also defined the predicates
```pl asc_desc(A, D)```
and ```pl asc_desc(A, D, P)```.
Both have a similar meaning
(thus we gave them the same name),
it is true when
```pl A``` is an ascendant of ```pl D```
(which is a descendant of ```pl A```)
and ```pl P``` is a list
denoting a path from ```pl A``` to ```pl D```
(without including ```pl A``` and ```pl D```).
In order to define
these predicates
in prolog system,
we could provide a list of facts
(similarly to how we defined
```pl parent_child/2``` and ```pl male/1```).
However,
it would be a long list of facts
which depends on
the previously defined predicate
```pl parent_child/2```.
Instead, we define both
```pl asc_desc/2``` and ```pl asc_desc/3```
with logical rules using
```pl parent_child/2```.

The definition for ```pl asc_desc/2```
reads as
"```pl A``` is an ascedent of ```pl D```
is true when
```pl A``` is the parent of ```pl X```
and either
```pl X``` is ```pl D```
or
```pl X``` is an ascedent of ```pl D```".
The definition for
```pl asc_desc/3```
is identical to
```pl asc_desc/2```'s definition,
but it includes a path of intermediary
ascendants/descendants.
Notice that both definitions
are recursive and
that is not a problem for Prolog.
We show answers to the queries
```pl ?- asc_desc(lara, X).```
and
```pl ?- asc_desc(lara, X, P).```,
meaning
"which Xs are descendant of Lara
(and who are the intermediary descendants)?"
in @repl:familytree.

== From Prolog to RDF and SPARQL Query Language <sec:prolog-to-semweb>

A RDF database describes
a directed graph
with labeled edges,
represented as
a set of triples of
subject (node),
predicate (edge-label),
and object (node).
As their names suggest,
each triple
asserts a truth:
the predicate holds for
the subject and the object.

#codefig(
  caption: [Family Tree in Turtle],
)[#{
  raw(lang: "ttl", read(resource("1familytree.ttl")));
}] <prog:familytree-ttl>

In @prog:familytree-ttl,
we show a RDF database
(serialized in turtle#footnote(links.turtle);)
which asserts
the same information
in @prog:familytree-pl.
We translate prolog binary predicates,
such as ```pl parent_child/2```,
to the triple
which
the subject is the first argument,
the predicate label is the prolog's predicate name,
and
the object is the second argument.
For instance,
the prolog fact
```pl parent_child(sergio, milton)```
is translated to the triple
```ttl :sergio :parent_child :milton```.
There is a convention in Semantic Web
to try to make phrases with the triple,
for instance,
```ttl :sergio :isParentOf :milton```.
We have decided to keep the prolog names
for now
to emphasize the similarities.

The unary predicates,
such as ```pl male/1``` and ```pl female/1```,
in essence,
do not relate two individuals from the world,
instead,
these predicates classify or give some attribute
to an individual.
Thus,
we use another strategy to translate them.
We translate prolog unary predicates
to the triple
which
the subject is the first argument,
the predicate label is the special edge ```ttl isA```,
and
the object is the predicate name.
The ```ttl isA``` is a special predicate
linking an individual to its class.
(In RDFS#footnote(links.rdfs);,
this special predicate is called ```ttl rdf:type```.)

In this translation
from Prolog to RDF,
it becomes clear that
the RDF framework
is reifying the predicates.
In another words,
we can make assertions and queries
about predicates in this framework.
In @prog:familytree-ttl,
the predicates
```ttl :male``` and ```ttl :female```
are objects of some triples.
We will talk more about reification
in @sec:semweb-to-prolog.
While in @prog:familytree-ttl,
predicates and individuals
are distinct things.

Using the database
defined in @prog:familytree-ttl,
we can use SPARQL queries
to ask questions about the database.
In SPARQL,
we ask yes or no questions
with the ```sparql ASK``` queries
and ask "which Xs" questions
with the ```sparql SELECT``` queries.
SPARQL~1.1 can handle
some recursive graph descriptions
with property paths.
We use property paths
to retrieve the descendants of Lara.
However, it is not possible to
retrieve the path from Lara to the descendant
without extensions to SPARQL~1.1.
See the translated SPARQL queries
in @prog:familytree-sparql.

Notice that
we do not explicitly show
the predicate ```pl asc_desc/2```,
defined in @prog:familytree-pl,
in the RDF database (@prog:familytree-ttl),
neither in SPARQL queries (@prog:familytree-sparql).
This happens because ```pl asc_desc/2```
is in essence some sort of subquery to the database
and SPARQL does not provide a way to name subqueries.
Inspite of that,
we are still able to ask
"which Xs are descendant of Lara?"
in SPARQL,
by instantiating some variables
in a more general query:
"which Xs are descendant of which Ys".

== From RDF and SPARQL Query Language back to Prolog <sec:semweb-to-prolog>

As we saw in @sec:prolog-to-semweb,
RDF reifies the predicates
to fit in their triple model.
This reification
adds some expressive power
to the description language.
With reified predicates,
we can give descriptions
to them,
for instance,
assert their domain and range.
Prolog programs
can achieve
this extra expressive power
without language extensions.
However,
it is not a usual requirement
for most programs
and this practice is avoided
because it incurs
some performance penalties
(since the predicate is no longer
#emph[hardcoded] nor #emph[indexed]).

To reify predicates in Prolog,
we create a new predicate
```pl rdf(Sub, Pred, Obj)```
to hold the triples.
For the binary predicates,
```pl Sub``` and ```pl Obj```
are the old arguments
and
```pl Pred```
is the predicate name.
And for the unary predicates,
we use the same trick
we did
in @sec:prolog-to-semweb,
to turn them into binary predicates.
For example,
we show the reification of
@prog:familytree-pl
in @prog:reif-familytree-pl.

The predicates
```pl asc_desc/2``` and ```pl asc_desc/3```
are the transitive closures
of the predicate ```pl parent_child/2```.
They appear generalized
in @prog:reif-familytree-pl,
respectively,
```pl plus(Sub, Pred, Obj)```
and
```pl plus(Sub, Pred, Obj, P)```.
The new predicates
define the transitive closure
of ```pl Pred```.
We name them ```pl plus```,
because
the ```sparql +```
in a SPARQL property path
denotes the transitive closure.
More generally,
one could implement
a full SPARQL's property path interpreter
in Prolog.

ISO-standard compatible prolog implementations
provide builtin predicates to collect
the results of a query into a prolog list;
these predicates are
```pl findall/3```, ```pl bagof/3```, and ```pl setof/3```.
We can use these predicates
to collect the results of a query
and use those results
to implement other SPARQL features,
such as
```sparql DESCRIBE``` query form,
```sparql MINUS```,
aggregation primitives
and
subqueries.

= The Prolog Interface

The prolog interface we propose
is inspired by Cliopatria's interface~#cite(<cliopatria>).
However,
ours put further emphasis on the following:
knowledge graph construction;
queries with explicit graphs;
and
meta information about the RDF structure.

The ability to construct knowledge graphs
enables prolog programs
to derive new knowledge.
For instance,
one could
enhance some existing graph
with new triples
infered from an ontology description.
Designing the query interface
to allow specifying
the search graph,
opens the possibility to
make multi-graph queries.
And finally,
by adding some
meta information about the RDF structure;
for example,
if a node is an IRI or a blank node;
we can take advantage of Prolog's syntax
for pattern matching
(and indexing),
instead of
converting back and forward
from atoms to strings.

We will describe the interface proposal in three parts:
resource and statement representation (@sec:interface-resource),
graph construction (@sec:interface-graph-construction),
and
graph querying (@sec:interface-graph-query).

== Resource and Statement Representation <sec:interface-resource>

A resource in a RDF graph is either a IRI, a blank node or a literal.
Thus, we represent a resource with one of the following functors:
```pl iri(Iri)```,
```pl blank(Labeled, Name)```, or
```pl literal(Type, Repr)```.

For the IRI case
```pl iri(Iri)```,
the inner value ```pl Iri```
must be an atom
which represents an IRI.
For instance,
```pl iri('http://example.org/alice')```,
and
```pl iri('mailto:alice@example.org')```.
```pl Iri``` should be compatible with
Cleopatria's representation of an IRI.

For the blank node case
```pl blank(Labeled, Name)```,
the first inner value ```pl Labeled```
is either
```pl labeled``` or ```pl unlabeled```.
If ```pl Labeled = labeled```,
then the second inner value ```pl Name```
is an atom starting with `_:`,
for instance,
```pl '_:a'```, ```pl '_:foo'```, and ```pl '_:123'```.
Otherwise ```pl Labeled = unlabeled```,
and the second inner value ```pl Name```
is any ground Prolog term.

Originally,
RDF does not make
this distinction on blank nodes.
However,
our intent is to use
```pl labeled``` blank nodes
when a blank node is read from a database
(where the labels usually are already provided),
and use the ```pl unlabeled``` blank nodes
when a prolog program needs to generate those labels.
This approach not only allows a prolog program
to defer
the task of generating names
which will not conflict with existing ones,
but also allows to
embed arbitrary information into
the blank node.

Finally, for the literal case
```pl literal(Type, Repr)```,
the first inner value ```pl Type```
is an IRI resource,
i.e. ```pl Type = iri(Iri)```,
where ```pl Iri```
represents the type of the literal.
If ```pl Type``` represents a language-tagged string,
i.e. ```pl Iri = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'```,
then ```pl Repr = @(Str, Lang)```,
where ```pl Str``` is a prolog string
with the language tag ```pl Lang```
which is also a prolog string.
Otherwise
the literal is not a language-tagged string,
and the second inner value ```pl Repr```
is a prolog string
which is a string representation of the literal.

The RDF framework considers
literals with the same type,
but different underlying string representations,
to be different nodes.
For instance,
using
```pl IntegerType = 'http://www.w3.org/2001/XMLSchema#integer'```,
the literals
```pl literal(IntegerType, "1")```
and
```pl literal(IntegerType, "01")```
are considered different,
despite both having the same value.
Therefore,
it is important to capture
this behavior in our prolog representation of literals.

In RDF,
a statement is
a triple of resources;
respectively,
subject, predicate, and object;
where
the subject is either an IRI or a blank node,
the predicate is an IRI, and
the predicate is either an IRI, a blank node, or a literal.
We represent a triple in prolog
with the functor
```pl t(Sub, Pred, Obj)```
where
```pl Sub```, ```pl Pred```, and ```pl Obj```
are, respectively,
the subject, the predicate, and the object
of the triple.

== Graph Construction <sec:interface-graph-construction>

A RDF graph is a set of triples.
Because of this, our proposal for
a graph construction interface is,
in essence,
a set construction interface.
We show the full interface
in @table:interface-graph-construction.

#figure(
  caption: [Graph Construction Interface],
)[#{
  let interface = (
    "empty_graph/1": (
      description: "is true iff `G` is an empty graph.",
      modes: (
        (
          mode: "empty_graph(+G)",
          is: "semidet",
        ), (
          mode: "empty_graph(-G)",
          is: "det",
        )
      ),
    ),
    "is_graph/1": (
      description: "is true iff `G` is a graph.",
      modes: (
        (
          mode: "is_graph(+G)",
          is: "semidet",
        ),
      ),
    ),
    "list_to_graph/2": (
      description: "is true iff `G` is a graph containing exactly the triples in the list `L`.",
      modes: (
        (
          mode: "list_to_graph(+L, -G)",
          is: "det",
        ),
      ),
    ),
    "graph_to_list/2": (
      description: "is true iff `L` is a list of triples representing the graph `G`.",
      modes: (
        (
          mode: "graph_to_list(+G, -L)",
          is: "det",
        ),
      ),
    ),
    "put_spo_graph/5": (
      description: "is true iff the graph `G0` with the triple `t(Sub, Pred, Obj)` is the graph `G`.",
      modes: (
        (
          mode: "put_spo_graph(+Sub, +Pred, +Obj, +G0, -G)",
          is: "det",
        ),
      ),
    ),
    "put_triple_graph/3": (
      description: "is true iff the graph `G0` with the triple `T` is the graph `G`.",
      modes: (
        (
          mode: "put_triple_graph(+T, +G0, -G)",
          is: "det",
        ),
      ),
    ),
    "del_spo_graph/5": (
      description: "is true iff the graph `G0` without the triple `t(Sub, Pred, Obj)` is the graph `G`.",
      modes: (
        (
          mode: "put_spo_graph(+Sub, +Pred, +Obj, +G0, -G)",
          is: "det",
        ),
      ),
    ),
    "del_triple_graph/3": (
      description: "is true iff the graph `G0` without the triple `T` is the graph `G`.",
      modes: (
        (
          mode: "put_triple_graph(+T, +G0, -G)",
          is: "det",
        ),
      ),
    ),
  );
  table_interface(interface);
}] <table:interface-graph-construction>

Our interface handles well
two kinds of workflows.
In the first workflow,
the library user starts
by creating an empty graph
with ```pl empty_graph/1```,
then he repeatedly
adds triples
until the graph description is completed.
In the second workflow,
the library user starts
with a graph already defined
is some representation
(perhaps it was serialized),
then he transforms it into a list of triples
and use ```pl list_to_graph/2```
to convert it into a graph.
Then at a latter moment
(comming from either workflow),
the user may convert the graph back into
a list of triples representation
using ```pl graph_to_list/2```,
and store it in his preferred serialization format.

Our interface provides
two kinds of predicates
for adding and removing triples.
The first kind
(```pl put_spo_graph/5``` and ```pl del_spo_graph/5```)
works with the subject, predicate and object
separately;
while the second kind
(```pl put_triple_graph/3``` and ```pl del_triple_graph/3```)
works with triple.

In the column Modes,
we specify the supported modes
of use of a predicate.
The arguments of the predicate
are prefixed with a
```pl +```, ```pl -```, or ```pl ?```
indicating if this argument is,
respectively,
instantiated (an "input" argument),
uninstantiated (an "output" argument),
or
either (it does not matter if it is instantiated or not).
The word after ```pl is```
is a determinism declaration of the predicate,
i.e. how many times the predicate may succeed.
For the purposes of this interface,
we have:
```pl semidet```
succeeds 0 or 1 times;
```pl det```
succeeds exactly once;
and
```pl nondet```
succeeds 0 or more times.

We remark that this interface is not minimal.
For instance,
one could implement ```pl list_to_graph/2```
by using ```pl foldl/4```
with ```pl put_triple_graph/3```,
the input list,
an empty graph (from ```pl empty_graph/1```)
and
the output graph
(this is how we implement
```pl list_to_graph/2```
in the reference implementation).

== Graph Querying <sec:interface-graph-query>

The querying interface is very simple,
containing one predicate and its mirror.
We describe it
in @table:interface-graph-query.
The predicate ```pl graph_spo/4```
tries to match
a subject, predicate and object
to a triple in the graph.
In essence,
```pl graph_spo/4```
does the same as the ```pl rdf/3```
shown in @sec:semweb-to-prolog
and used in Cliopatria's interface~#cite(<cliopatria>),
but it expects the graph as the first argument.
The predicate ```pl graph_triple/2```
does the same as
```pl graph_spo/4```
but it works with triples.

#figure(
  caption: [Graph Querying Interface],
)[#{
  let interface = (
    "graph_spo/4": (
      description: "is true iff `t(S, P, O)` is a triple in graph `G`.",
      modes: (
        (
          mode: "graph_spo(+G, +S, +P, +O)",
          is: "semidet",
        ), (
          mode: "graph_spo(+G, ?S, ?P, ?O)",
          is: "nondet",
        )
      ),
    ),
    "graph_triple/2": (
      description: "is true iff `T` is a triple in graph `G`.",
      modes: (
        (
          mode: "graph_triple(+G, +T)",
          is: "semidet",
        ), (
          mode: "graph_triple(+G, ?T)",
          is: "nondet",
        )
      ),
    ),
  );
  table_interface(interface);
}] <table:interface-graph-query>

= The Accompanying Implementation

Alongside with the interface description,
we provide
a reference implementation
using unordered lists of triples,
a non-production-ready turtle parser implementation,
and
some tests cases for both.
All the code is available
at the repository:
#repo.

The reference implementation resides
in the file `semweb_unord_lists.pl`.
We implemented the reference library
using some list builtins
and predicates from `library(reif)`~#cite(<indexingdif>).
The implementation is straight forward,
because of this,
we will not talk about it any further.

== Turtle Parser

We used Definite Clause Grammars (DCGs)
to implement
the turtle parser
and it resides
in the file `turtle.pl`.
Using this parser,
we implemented
two utility scripts:
`ttlcat.pl`
which expects a file and
prints to stdout
each triple in its own line
(expanding base and prefixes);
and
`ttl2pl.pl`
which expects a file and
prints to stdout
each triple in the format
we specified in @sec:interface-resource
(with a final dot/period,
so that one could easily read each triple
with ```pl read/1```).

We do not consider
the parser implementation to be production-ready
for some reasons.
The first reason is that
it throws debuging errors
on syntax errors and on backtracking.
Secondly,
it does not detects a relative IRI
for substituting the base;
instead,
it always assumes that the IRI is relative
and prepends the base.
For instance,
if the base is set to `http://example.org/base/`,
and we read the IRIs
`thing`
and
`http://example.org/thing`,
the parser will incorrectly produce
`http://example.org/base/thing`
and
`http://example.org/base/http://example.org/thing`.
And finally,
it does not do any IRI normalization.
For instance,
`http://example.org/base#fragment`
and
`http://example.org/base/#fragment`
are treated as different IRIs
(but they are the same).

= Ideas for Extending the Interface

We have some ideas
for making the interface
more convenient to users.
We could add more set operations
to the graph construction interface,
such as,
union, intersection, and minus.
These operations would
allow implementations
to provide more preformative alternatives
for those operations,
compared to the composition of
the primitives already provided.

Another ideia
is to provide support for prefixes.
In some occasions,
it would be nicer to
have the possibility to
interface with prefixes,
such as
```pl xsd:integer```,
instead of a full IRI.
Cliopatria has support to prefixed IRIs.

The last idea is
to define a DSL to describe the queries.
The existence of a DSL
(similar to DCGs)
would allow an implementation to:
interpret the query
(allowing to implement SPARQL's ```sparql LIMIT```);
optimize the query before execution
(similarly to Cliopatria's ```pl rdf_optimize/2```);
translate queries to and from SPARQL,
which shortens the path to implement federated queries.

= Figures, Programs and Interactions

#repl(
  caption: [Some Queries and Answers from @prog:familytree-pl],
)[```pl
% is Helena male?
?- male(helena).
   false.

% which Xs are male?
?- male(X).
   X = sergio           ;  X = milton           ;  X = george
;  X = mario            ;  X = alexandre        ;  X = andre.

% is Carmem parent of Ema and Ema female?
?- parent_child(carmem, ema), female(ema).
   true.

% which Xs are the daughters of Carmem?
?- parent_child(carmem, X), female(X).
   X = sara             ;  X = ema.

% which Xs are descendant of Lara?
?- asc_desc(lara, X).
   X = milton                       ;  X = george
;  X = ana                          ;  X = andre
;  X = carmem                       ;  X = sara
;  X = ema                          ;  false.

% which Xs are descendant of Lara and who are the intermediary descendants?
?- asc_desc(lara, X, P).
   X = milton, P = []               ;  X = george, P = [milton]
;  X = ana,    P = [milton,george]  ;  X = andre,  P = [milton,george]
;  X = carmem, P = [milton]         ;  X = sara,   P = [milton,carmem]
;  X = ema,    P = [milton,carmem]  ;  false.
```] <repl:familytree>

#codefig(
  caption: [Some SPARQL Queries to @prog:familytree-ttl]
)[#{
  let gutter = 0.5em;
  let sep = { v(gutter); line(length: 75%, stroke: 0.25pt + color.gray); v(gutter); };
  let queries = (
    ```sparql
# is Helena male?
ASK { :helena :isA :male . }
    ```, ```sparql
# which Xs are male?
SELECT ?x WHERE { ?x :isA :male . }
    ```, ```sparql
# is Carmem parent of Ema and Ema female?
ASK { :carmem :parent_child :ema . :ema :isA :female . }
    ```, ```sparql
# which Xs are the daughters of Carmem?
SELECT ?x { :carmem :parent_child ?x . ?x :isA :female . }
    ```, ```sparql
# which Xs are descendant of Lara?
SELECT ?x { :lara :parent_child+ ?x . }
    ```, ```sparql
# which Xs are descendant of which Ys?
SELECT ?y ?x { ?y :parent_child+ ?x . }
    ```,
  );
  stack(dir: ttb,
    ..queries.intersperse(sep)
  )
}] <prog:familytree-sparql>

#codefig(
  caption: [Reified Family Tree in Prolog],
)[#{
  raw(lang: "pl", read(resource("2familytree.pl")));
}] <prog:reif-familytree-pl>
