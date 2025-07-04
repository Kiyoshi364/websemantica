#let links = (
  turtle: link("https://www.w3.org/TR/2014/REC-turtle-20140225/"),
  rdf-primer: link("https://www.w3.org/TR/2014/NOTE-rdf11-primer-20140624/"),
  rdf-concepts: link("https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/"),
  rdfs: link("https://www.w3.org/TR/2014/REC-rdf-schema-20140225/"),
  sparql11: link("https://www.w3.org/TR/2013/REC-sparql11-query-20130321/"),
);

#let resource(name) = "resources/" + name;

#let syntaxes = (
  resource("ISO-Prolog.sublime-syntax"),
  resource("ttl.sublime-syntax"),
);

#let codefig = figure.with(kind: "code", supplement: [Program]);
#let repl = figure.with(kind: "repl", supplement: [Interaction]);
