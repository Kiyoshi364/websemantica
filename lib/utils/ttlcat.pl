:- use_module(library(format), [format_//2]).
:- use_module(library(lists), [foldl/4]).
:- use_module(library(pio), [phrase_from_stream/2, phrase_to_stream/2]).
:- use_module(library(iso_ext), [setup_call_cleanup/3]).

:- use_module('../parsers/turtle', [
  initial_pos//0,
  empty_state/1, parse//2
]).

read_triples(Stream, Ts) :-
  phrase_from_stream(parse(Ts, _), Stream), !.

run_file(File) :-
  setup_call_cleanup(
    open(File, read, IStream, []),
    read_triples(IStream, Ts),
    close(IStream)
  ),
  current_output(OStream),
  phrase_to_stream(foldl(triple, Ts), OStream).

triple(t(S, V, O)) --> node(S), " ", node(V), " ", node(O), "\n".

node(resource(T, X)) --> resource(T, X).
node(literal(resource(T, Type), V)) --> literal(V, T, Type).

resource(iri, Iri) --> format_("<~a>", [Iri]).
resource(blank(L), N) --> blank(L, N).

blank(labeled, N) --> format_("~q", [N]).
blank(unlabeled, N) --> format_("_:~N", [N]).

literal(@(Str, Lang), _, _) --> format_("\"~s\"@~s", [Str, Lang]).
literal([S|Tr], T, Type) --> format_("\"~s\"^^", [[S|Tr]]), resource(T, Type).
