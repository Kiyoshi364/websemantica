:- use_module(library(format), [format_/2]).
:- use_module(library(dcgs), []).
:- use_module(library(lists), [foldl/4]).
:- use_module(library(pio), [phrase_to_file/2]).

:- initialization(consult('0familytree.pl')).

gengraph(File) :- phrase_to_file(family_graph, File).

family_graph -->
  "digraph {\n",
  parents,
  males,
  females,
  "}".

males -->
  { findall(P, male(P), Ps) },
  foldl(male, Ps).
male(P) -->
  "    ", person(P), "[shape=box];\n".

females -->
  { findall(P, female(P), Ps) },
  foldl(female, Ps).
female(P) -->
  "    ", person(P), "[shape=oval];\n".

parents -->
  { findall(P-F, parent_child(P, F), PFs) },
  foldl(parent_child, PFs).

parent_child(P-F) -->
  "    ", person(P), " -> ", person(F), ";\n".

person(X) --> format_("~a", [X]).
