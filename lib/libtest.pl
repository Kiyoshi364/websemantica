:- module(libtest, [
  prefix_module_arity_predicates/4,
  run_log_and_halt/2, run_log_and_halt/3,
  run_tests/2, run_tests/3,
  run_test/4, run_test/5
]).

:- use_module(library(lists), [append/3, foldl/4]).
:- use_module(library(iso_ext), [call_cleanup/2]).

prefix_module_arity_predicates(Prefix, M, A, Ps) :-
  ( findall(T,
      ( T = M:P,
        current_predicate(M:P/A),
        atom_chars(P, Name),
        append(Prefix, _, Name)
      ),
      Ps
    )
  ; Ps = []
  ).

run_log_and_halt(M, Ts) :-
  foldl(run_test(M), Ts, t(0, 0, 0, 0, 0), t(Np, Nf, Nd, Nds, Nt)),
  run_halt(Np, Nf, Nd, Nds, Nt).

run_log_and_halt(M, Ctx, Ts) :-
  foldl(run_test(M, Ctx), Ts, t(0, 0, 0, 0, 0), t(Np, Nf, Nd, Nds, Nt)),
  run_halt(Np, Nf, Nd, Nds, Nt).

run_halt(Np, Nf, Nd, Nds, Nt) :-
  ( (Nf > 0 ; Nd > 0) -> nl; true ),
  ( Nf > 0 ->
    write(Nf), writen(' tests failed!')
  ; true
  ),
  ( Nd > 0 ->
    write(Nd), writen(' tests were not well-behaved deterministic!')
  ; true
  ),
  ( Np > 0 ->
    write(Np), writen(' tests passed.')
  ; writen('No tests to run.')
  ),
  ( Nds > 0 ->
    write(Nds), writen(' tests not well-behaved deterministic supreessed.')
  ; true
  ),
  ( Nt == Np -> ExitCode = 0 ; ExitCode = 1 ),
  halt(ExitCode),
true.

run_tests(Prefix, Module) :-
  prefix_module_arity_predicates(Prefix, Module, 0, Ts),
  run_log_and_halt(Module, Ts),
true.

run_tests(Prefix, Module, Ctx) :-
  prefix_module_arity_predicates(Prefix, Module, 1, Ts),
  run_log_and_halt(Module, Ctx, Ts),
true.

:- meta_predicate(run_test(?, 0, ?, ?)).
:- meta_predicate(run_test(?, ?, 0, ?, ?)).

run_test(M, Test, t(Np0, Nf0, Nd0, Nds0, Nt0), t(Np, Nf, Nd, Nds, Nt)) :-
  Nt is Nt0 + 1,
  catch(
    ( call_cleanup(Test, Result = pass) ; Result = failed ),
    Err,
    ( Err = error('$interrupt_thrown', repl/0) -> throw(Err)
    ; Result = failed,
      write(Test), write(' throws exception '), write(Err), nl
    )
  ),
  ( Result == pass ->
    Np is Np0 + 1, Nf is Nf0, Nd is Nd0, Nds is Nds0,
    ( M:nwdet_ok(Test) ->
      write(Test), write(' IS WELL-BEHAVED DETERMINISTIC!'), nl
    ; true
    )
  ; Result == failed ->
    Np is Np0, Nf is Nf0 + 1, Nd is Nd0, Nds is Nds0,
    write(Test), write(' failed!'), nl
  ; var(Result) ->
    ( M:nwdet_ok(Test) ->
      Np is Np0 + 1, Nf is Nf0, Nd is Nd0, Nds is Nds0 + 1
    ; Np is Np0, Nf is Nf0, Nd is Nd0 + 1, Nds is Nds0,
      write(Test), write(' is not well-behaved deterministic!'), nl
    )
  ),
  !.

run_test(M, Ctx, Test, t(Np0, Nf0, Nd0, Nds0, Nt0), t(Np, Nf, Nd, Nds, Nt)) :-
  Nt is Nt0 + 1,
  catch(
    ( call_cleanup(call(Test, Ctx), Result = pass) ; Result = failed ),
    Err,
    ( Err = error('$interrupt_thrown', repl/0) -> throw(Err)
    ; Result = failed,
      write(Test), write(' throws exception '), write(Err), nl
    )
  ),
  ( Result == pass ->
    Np is Np0 + 1, Nf is Nf0, Nd is Nd0, Nds is Nds0,
    ( M:nwdet_ok(Test) ->
      write(Test), write(' IS WELL-BEHAVED DETERMINISTIC!'), nl
    ; true
    )
  ; Result == failed ->
    Np is Np0, Nf is Nf0 + 1, Nd is Nd0, Nds is Nds0,
    write(Test), write(' failed!'), nl
  ; var(Result) ->
    ( M:nwdet_ok(Test) ->
      Np is Np0 + 1, Nf is Nf0, Nd is Nd0, Nds is Nds0 + 1
    ; Np is Np0, Nf is Nf0, Nd is Nd0 + 1, Nds is Nds0,
      write(Test), write(' is not well-behaved deterministic!'), nl
    )
  ),
  !.

writen(X) :- write(X), nl.
