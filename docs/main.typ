#import "fine-lncs/lib.typ": lncs, institute, author;

#let inst_ic = institute(
  "Instituto de Computação",
  addr: "Universidade Federal do Rio de Janeiro, Brasil",
  url: "https://ufrj.br/en/",
);

#let abstract = [
  Semantic Web attempts to
  bring machine-readable information
  to the web
  by using technologies such as
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
  we ask questions to the Prolog system about these knowledge
  to find out on which conditions it is true.
  We can represent
  a RDF database and SPARQL queries
  with Prolog's facts and queries,
  respectively.
  In this report,
  we propose
  an implementation-independent Prolog interface
  for reasoning about semantic web databases
  and provide a referencence implementation
  for the this interface.
];

#let keywords = (
  "Semantic Web Framework",
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
  bibliography: bibliography("refs.bib")
);

#let links = (
  turtle: link("https://www.w3.org/TR/2014/REC-turtle-20140225/"),
  rdf-primer: link("https://www.w3.org/TR/2014/NOTE-rdf11-primer-20140624/"),
  rdf-concepts: link("https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/"),
  rdfs: link("https://www.w3.org/TR/2014/REC-rdf-schema-20140225/"),
  sparql11: link("https://www.w3.org/TR/2013/REC-sparql11-query-20130321/"),
);

#let resource(name) = "resources/" + name;

#set document(keywords: keywords);
#set raw(syntaxes: (
  resource("ISO-Prolog.sublime-syntax"),
  resource("ttl.sublime-syntax"),
));

#let codefig = figure.with(kind: "code", supplement: [Program]);
#let repl = figure.with(kind: "repl", supplement: [Interaction]);

= Introduction

#lorem(100)
#cite(<cliopatria>)

#cite(<triplestoresurvey>)

= Prolog and RDF

Before we describe the interface,
we give a brief introduction to
Prolog, RDF
and how RDF databases
may be represented in Prolog.
For that,
we will provide an prolog example
then show a translation of this example
to RDF and SPARQL queries.

== Prolog Language

Prolog programming
consists of describing what is true
by stating truth facts and
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

#codefig(
  caption: [Family Tree in Prolog],
)[#{
  raw(lang: "pl", read(resource("0familytree.pl")));
}] <prog:familytree-pl>

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
These predicates is represented
in @fig:familytree:
the arrows points from the parent to the child,
males are drawn in a box,
and
females are drawn in an oval.

#figure(
  caption: [Family Tree Representation],
)[#{
  image(resource("familytree.dot.svg"))
}] <fig:familytree>

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

#repl(
  caption: [Some Queries and Answers from @prog:familytree-pl to Scryer Prolog],
)[```pl
% is Helena male?
?- male(helena).
   false.

% which Xs are male?
?- male(X).
   X = sergio           ;  X = milton           ;  X = george
;  X = mario            ;  X = alexandre        ;  X = andre.
?- parent_child(carmem, ema), female(ema).
   true.

% is Carmem parent of Ema and Ema female?
?- parent_child(carmem, X), female(X).
   X = sara             ;  X = ema.

% which Xs are the daughters of Carmem?
?- asc_desc(lara, X).
   X = milton                       ;  X = george
;  X = ana                          ;  X = andre
;  X = carmem                       ;  X = sara
;  X = ema                          ;  false.

% which Xs are descendant of Lara?
?- asc_desc(lara, X, P).
   X = milton, P = []               ;  X = george, P = [milton]
;  X = ana,    P = [milton,george]  ;  X = andre,  P = [milton,george]
;  X = carmem, P = [milton]         ;  X = sara,   P = [milton,carmem]
;  X = ema,    P = [milton,carmem]  ;  false.
```] <repl:familytree>

== From Prolog to RDF and SPARQL Query Language

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
describes a truth:
the predicate holds for
the subject and the object.

In @prog:familytree-ttl,
we show a RDF database
(serialized in turtle#footnote(links.turtle);)
which describes
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

#codefig(
  caption: [Family Tree in Turtle],
)[#{
  raw(lang: "ttl", read(resource("1familytree.ttl")));
}] <prog:familytree-ttl>

In this translation
from Prolog to RDF,
it becomes clear that
the RDF framework
is reifying the predicates.
In another words,
we can make statements
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
in @prog:sparql-familytree.

#codefig(
  caption: [Some SPARQL Queries from @prog:familytree-ttl]
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
    ```,
  );
  stack(dir: ttb,
    ..queries.intersperse(sep)
  )
}] <prog:sparql-familytree>

== From RDF and SPARQL Query Language back to Prolog <sec:semweb-to-prolog>

= The Prolog Interface

= The Reference Implementation

== Tests
== Turtle Parser

= Conclusion

== Future Work

== asdf
