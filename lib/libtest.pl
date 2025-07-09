/** libtest.pl

  This is a simple library to define and run tests.

  # Basic Usage

  To use, add the following to the top of the file:
  ```prolog
  :- use_module(libtest).

  nwdet_ok(_:T) :- nwdet(T).
  :- discontiguous(nwdet/1).
  ```

  Tests are predicates are usual prolog predicates:
  ```prolog
  test_unification_with_one :- _ = one.
  test_2_is_1_plus_1 :- 2 is 1 + 1.
  test_failing :- one = two.
  test_throws_error :- throw(error(bad_test)).
  ```

  To run call `run_tests/2`,
  passing a prefix for your tests and the current module:
  ```prolog
  ?- run_tests("test_", the_current_module).
  ```

  The test is ran until its first success,
  detecting if the test leaves a choice point.
  To suppress these warnings define `nwdet/1` for your test.
  ```prolog
  test_leaving_choice_point_with_warning :- (X = 1 ; X = 2).

  nwdet(test_leaving_choice_point_without_warning).
  test_leaving_choice_point_without_warning :- (X = 1 ; X = 2).
  ```

  For more information,
  read the implementation for the exported predicates.
  Feel free to hack the code to solve your own problem.

  LICENSE

    See end of file for license information.

  MIRRORS

    No mirrors currently.

  CHANGELOG

    [Semantic Versioning](https://semver.org/spec/v2.0.0.html) is implied

    2025-07-09 v0.0      First Release
*/
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
    ) -> true
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
  ; true
  ),
  ( Nds > 0 ->
    write(Nds), writen(' tests not well-behaved deterministic supreessed.')
  ; true
  ),
  ( Nt =< 0 ->
    writen('No tests to run.')
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
  catch(
    ( call_cleanup(Test, Result = pass) ; Result = failed ),
    Err,
    ( Err = error('$interrupt_thrown', repl/0) -> throw(Err)
    ; Result = failed,
      write(Test), write(' throws exception '), write(Err), nl
    )
  ),
  report_test_result(M, Test, Np0, Nf0, Nd0, Nds0, Nt0, Np, Nf, Nd, Nds, Nt, Result),
  !.

run_test(M, Ctx, Test, t(Np0, Nf0, Nd0, Nds0, Nt0), t(Np, Nf, Nd, Nds, Nt)) :-
  catch(
    ( call_cleanup(call(Test, Ctx), Result = pass) ; Result = failed ),
    Err,
    ( Err = error('$interrupt_thrown', repl/0) -> throw(Err)
    ; Result = failed,
      write(Test), write(' throws exception '), write(Err), nl
    )
  ),
  report_test_result(M, Test, Np0, Nf0, Nd0, Nds0, Nt0, Np, Nf, Nd, Nds, Nt, Result),
  !.

report_test_result(M, Test, Np0, Nf0, Nd0, Nds0, Nt0, Np, Nf, Nd, Nds, Nt, Result) :-
  Nt is Nt0 + 1,
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
  ).

writen(X) :- write(X), nl.
/*
  The MIT License (MIT)

  Copyright (c) 2025 Daniel K Hashimoto <dkhashimoto@ic.ufrj.br>

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use,
  copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and
  to permit persons to whom the Software is furnished to do so,
  subject to the following conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
  ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
  CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
