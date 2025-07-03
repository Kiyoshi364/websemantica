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
    author("Daniel K Hashimoto", 
      oicd: "0000-0002-3113-4488",
      insts: (inst_ic),
    ),
  ),
  abstract: abstract,
  keywords: keywords,
  bibliography: bibliography("refs.bib")
);

#set document(keywords: keywords);
#set figure(placement: auto);
#set raw(syntaxes: ("ISO-Prolog.sublime-syntax",));

#let prolog = figure.with(kind: "prolog", supplement: [Program]);
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

== The Prolog Language

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
consider the @prog:familytree.
Facts and rules are terminated with a period/dot (```pl .```).
For instance,
```pl parent_child(sergio, milton).```
is a fact
and
```pl asc_desc(A, D) :- parent_child(A, X), ( X = D ; asc_desc(X, D) ).```
is a rule.

#prolog(
  caption: [Family Tree],
)[#{
  set align(start);
  raw(lang: "pl", read("familytree/0familytree.pl"));
}] <prog:familytree>

@prog:familytree describes 3 predicates with facts
(```pl parent_child/2```,
```pl male/1```, and
```pl female/1```)
and 2 predicates with rules
(```pl asc_desc/2```, and
```pl asc_desc/3```).
The number after ```pl /```
indicates the arity (number of arguments) of the predicate
which is used to differentiate predicates.

The predicate ```pl parent_child/2```
indicates that the first argument
is the parent of the second argument
(which is a child of the first).
The lowercase names appearing
inside the parenthesis
(```pl sergio```,
```pl milton```,
```pl lara```,
...)
are atoms:
named constants
representing distict objects.
The predicate ```pl male/1```
indicates that its argument is male,
likewise,
the predicate ```pl female/1```
indicates that its argument is female.
These predicates is represented
in @fig:familytree:
the arrows points from the parent to the child,
males are drawn in a box,
and
females are drawn in an oval.

#figure(
  caption: [Family Tree Representation],
)[#{
  image("familytree/familytree.dot.svg")
}] <fig:familytree>

Before looking at the rules of @prog:familytree
we will take a look at queries
using the facts from this program.
In prolog,
we can make fully instantiated queries
(without any variables),
such as
```pl ?- male(helena).```,
and
```pl ?- parent_child(milton, carmem), female(carmem).```.
These queries act as yes or no questions
to the prolog system.
The previous queries mean:
"is Helena male?",
and
"is Milton parent of Carmem and Carmem female?".
However,
we can also make
partially instantiated queries
to prolog,
such as
```pl ?- male(X).```,
and
```pl ?- parent_child(milton, X), female(X).```.
In these queries
the prolog system will attempt
to substitute the variables
(which start with an uppercase letter)
to make the query true.
These queries mean:
"which X are male?",
and
"which X are child of Milton and are female?"
(alternatively, "which X are the daughters of Milton?").
We show the answers to all of these queries,
when asked to Scryer Prolog,
in @repl:familytree.

#repl(
  caption: [Examples of Query to @prog:familytree],
)[```pl
?- male(helena).
   false.
?- parent_child(milton, carmem), female(carmem).
   true.
?- male(X).
   X = sergio
;  X = milton
;  X = george
;  X = mario
;  X = alexandre
;  X = andre.
?- parent_child(milton, X), female(X).
   X = carmem.
```] <repl:familytree>

== The Resource Description Framework

= The Prolog Interface

= Reference Implementation

== Tests
== Turtle Parser

= Conclusion

== Future Work

== asdf
