/** libtest.pl

  if_/3 for DCGs.

  LICENSE

    See end of file for license information.
*/
:- module(reif_dcgs, [
  if_//3
]).

:- use_module(library(dcgs), [phrase/3]).
:- use_module(library(reif), [if_/3]).

:- meta_predicate(if_(1, 2, 2, ?, ?)).

if_(If_1, Then_2, Else_2) -->
  { call(If_1, T) },
  ( { T == true  } -> phrase(Then_2)
  ; { T == false } -> phrase(Else_2)
  ; { nonvar(T) } -> { throw(error(type_error(boolean, T), _)) }
  ; { throw(error(instantiation_error, _)) }
  ).
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
