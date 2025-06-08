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
* Optional: use `';'/2` (ex: `( ... ; true )`)

# SPARQL Datasets

Reason: too complicated

# Query Types

Reason:

* Select: is the default
* Ask: use `!/0`
* Construct: `findall/3` + `sort/2`
* Describe: ? // TODO

# Other features

Reason: generaly `findall/3` + ???

* Aggregation: `fold/4`
* Solution modifiers: ? // TODO
* Bag semantics: ? // TODO
* Federation: internet in complicated, requires "prolog+rdf" -> SPARQL mapping
* Entailment: ? // TODO

