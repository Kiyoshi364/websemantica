#let links = (
  turtle: link("https://www.w3.org/TR/2014/REC-turtle-20140225/"),
  rdf-primer: link("https://www.w3.org/TR/2014/NOTE-rdf11-primer-20140624/"),
  rdf-concepts: link("https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/"),
  rdfs: link("https://www.w3.org/TR/2014/REC-rdf-schema-20140225/"),
  sparql11: link("https://www.w3.org/TR/2013/REC-sparql11-query-20130321/"),
  scryer-prolog: link("https://www.scryer.pl/"),
);
#let gitlab(owner: "Hashi364", repo) = {
  let target = "gitlab.com/" + owner + "/" + repo;
  link("https://" + target, raw(target));
};
#let repo = gitlab("semweb");

#let resource(name) = "resources/" + name;

#let syntaxes = (
  resource("ISO-Prolog.sublime-syntax"),
  resource("ttl.sublime-syntax"),
);

// modified from: https://gist.github.com/hongjr03/5bc3f0c019a233450b82cdf583fbaa2c
#let show-raw-block = it => block(
)[#grid(
  columns: (1em, 1fr),
  align: (right, left),
  column-gutter: 0.7em,
  row-gutter: 0.45em,
  ..(if it.lines.last().text == "" {it.lines.slice(0, -1)} else {it.lines})
    .enumerate()
    .map(((i, line)) => (text(color.gray)[#(i + 1)], line))
    .flatten(),
)];

#let codefig = figure.with(kind: "code", supplement: [Program]);
#let repl = figure.with(kind: "repl", supplement: [Interaction]);

#let format_interface_entry(lang: "pl", entry) = {
  let predicate = entry.at(0);
  let description = entry.at(1).description;
  let modes = entry.at(1).modes;
  (
    table.hline(),
    table.cell(
      rowspan: modes.len(),
      align: center,
      raw(lang: lang, predicate),
    ),
    table.cell(
      rowspan: modes.len(),
      description,
    ),
    ..modes.map(m => raw(lang: lang, m.mode + " is " + m.is + "."))
      .intersperse(table.hline(stroke: luma(75%))),
    table.hline(),
  )
};
#let table_interface(interface) = table(
  columns: 3,
  inset: 5pt,
  align: horizon,
  table.header(
    table.hline(),
    [Predicate], [Description], [Modes],
  ),
  ..interface.pairs().map(format_interface_entry).flatten(),
);
