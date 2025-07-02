#import "fine-lncs/lib.typ": lncs, institute, author, theorem, proof

#let inst_ic = institute(
  "Instituto de Computação",
  addr: "Universidade Federal do Rio de Janeiro, Brasil",
  url: "https://ufrj.br/en/",
);

#let abstract = [
  The abstract should briefly summarize the contents of the paper in
  15--250 words.
];

#let keywords = (
  "Semantic Web Framework",
  "Logic Programming",
  "Prolog",
  "Triple Store",
  "SPARQL",
);

#show: lncs.with(
  title: "Towards a General Prolog Interface for Semantic Web",
  authors: (
    author("Daniel K Hashimoto", 
      oicd: "0000-0002-3113-4488",
      insts: (inst_ic),
    ),
  ),
  abstract: abstract,
  keywords: keywords,
  bibliography: bibliography("refs.yml")
);

#set document(keywords: keywords);

= First Section

My awesome paper ...
