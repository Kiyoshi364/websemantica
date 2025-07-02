:- use_module(library(lists), [foldl/4]).
:- use_module(library(pio), [phrase_from_stream/2, phrase_to_stream/2]).
:- use_module(library(iso_ext), [setup_call_cleanup/3]).

:- use_module(library(dcgs), [phrase/3]).
:- use_module(library(format), [format_//2]).
:- use_module(library(reif), [if_/3, (=)/3, memberd_t/3]).

:- meta_predicate(if_(1, 2, 2, ?, ?)).

if_(If_1, Then_2, Else_2) -->
  { call(If_1, T) },
  ( { T == true  } -> phrase(Then_2)
  ; { T == false } -> phrase(Else_2)
  ; { nonvar(T) } -> { throw(error(type_error(boolean, T), _)) }
  ; { throw(error(instantiation_error, _)) }
  ).

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

triple(t(S, V, O)) --> "t(", resource(S), ", ", resource(V), ", ", resource(O), ").\n".

resource(iri(Iri)) --> "iri(", format_("~q", [Iri]), ")".
resource(blank(L, N)) --> "blank(", format_("~q", [L]), ", ", format_("~q", [N]), ")".
resource(literal(Ty, V)) -->
  "literal(", resource(Ty), ", ",
  if_(V = @(S, L),
    ( "@(\"", prolog_string(S), "\", \"", prolog_string(L), "\")" ),
    ( "\"", prolog_string(V), "\"" )
  ),
  ")".

prolog_string([]) --> [].
prolog_string([H | T]) --> prolog_char(H), prolog_string(T).

prolog_char(C) --> if_(needs_escape_t(C), escape(C), [C]).

needs_escape_t(C, T) :- memberd_t(C, "\b\t\n\v\f\r\a\\", T).
escape(C) --> { char_code(C, X) }, [(\), x], hex(X), [(\)].

hex(X) -->
  { X1 is div(X, 16), X2 is mod(X, 16) },
  if_(X1 = 0, [], hex(X1)),
  [X2].
