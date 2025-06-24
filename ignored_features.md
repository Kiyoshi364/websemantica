# Navigational Graph Patters (NGPs)

Reason: can be implemented in Prolog

# Complex Graph Patters (CGPs)

Reason: can be implemented in Prolog

* Filter: prolog predicates
* Select: hide variables
  (define a predicate that do not expose all variables)
* Union: `findall/3` + `append/3`
* Exists: ? // TODO
* Minus: ? // TODO
* Optional: `optional(T) :- copy_term(T, T0, Gs0), maplist(call, Gs0), (call(T0) -> call(T) ; true)`

# SPARQL Datasets

Reason: too complicated

# Query Types

Reason:

* Select: is the default
* Ask: use `!/0` or `\+(\+ P)`
* Construct: `findall/3` + `sort/2`
* Describe: `findall/3` + `findall(t(S, P, O), ((S=X;P=X;O=X),rdf(S, P, O)), Ts)`

# Other features

Reason: generaly `findall/3` + ???

* Aggregation: `fold/4`
* Solution modifiers: ? // TODO
* Bag semantics: ? // TODO
* Federation: internet in complicated, requires "prolog+rdf" -> SPARQL mapping
* Entailment: ? // TODO

# Edge inferences

OWL rules (transitivity, symmetry, ...) and stuff

Reason: too complicated
