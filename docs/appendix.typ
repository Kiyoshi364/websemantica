#import "defs.typ": resource, syntaxes, show-raw-block, codefig, repl;

#let appendix = [
#set raw(block: true, syntaxes: syntaxes);
#show raw.where(block: true): show-raw-block;

= Figures, Programs and Interactions

#codefig(
  caption: [Family Tree in Prolog],
)[#{
  raw(lang: "pl", read(resource("0familytree.pl")));
}] <prog:familytree-pl>

#figure(
  caption: [Family Tree Representation],
)[#{
  image(resource("familytree.dot.svg"))
}] <fig:familytree>

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
  caption: [Family Tree in Turtle],
)[#{
  raw(lang: "ttl", read(resource("1familytree.ttl")));
}] <prog:familytree-ttl>

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

#codefig(
  caption: [Reified Queries and Answers from @repl:familytree],
)[```pl
% is Helena male?
?- rdf(helena, isA, male).
   false.

% which Xs are male?
?- rdf(X, isA, male).
   X = sergio           ;  X = milton           ;  X = george
;  X = mario            ;  X = alexandre        ;  X = andre.

% is Carmem parent of Ema and Ema female?
?- rdf(carmem, parent_child, ema), rdf(ema, isA, female).
   true.

% which Xs are the daughters of Lara?
?- rdf(carmem, parent_child, X), rdf(X, isA, female).
   X = sara             ;  X = ema.

% which Xs are descendant of Lara?
?- plus(lara, parent_child, X).
   X = milton                       ;  X = george
;  X = ana                          ;  X = andre
;  X = carmem                       ;  X = sara
;  X = ema                          ;  false.

% which Xs are descendant of Lara and who are the intermediary descendants?
?- plus(lara, parent_child, X, P).
   X = milton, P = []               ;  X = george, P = [milton]
;  X = ana,    P = [milton,george]  ;  X = andre,  P = [milton,george]
;  X = carmem, P = [milton]         ;  X = sara,   P = [milton,carmem]
;  X = ema,    P = [milton,carmem]  ;  false.
```] <prog:reif-familytree-queries>

#codefig(
  caption: [Property Path Interpreter in Prolog],
)[#{
  raw(lang: "pl", read(resource("path_exprs.pl")));
}] <prog:property-path-pl>

#codefig(
  caption: [SPARQL's ```sparql OPTIONAL```, ```sparql FILTER NOT EXISTS``` and ```sparql FILTER EXISTS``` in Prolog],
)[#{
  raw(lang: "pl", read(resource("sparql.pl")));
}] <prog:sparql-pl>

#codefig(
  caption: [Reference implementation with unorded lists],
)[#{
  raw(lang: "pl", read(resource("semweb_unord_lists.pl")));
}] <prog:reference-impl>
];
