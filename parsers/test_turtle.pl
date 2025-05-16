:- module(test_turtle, [
  run_tests/0, run_tests/1
]).

:- use_module(turtle, [
  token//1
]).

:- use_module(library(dcgs), [phrase/3]).
:- use_module(library(lists), [
  append/3, foldl/4, maplist/2, maplist/3, length/2
]).

:- use_module(linecol, [empty_pos/1]).

:- use_module(libtest).

repeated_phrase(GRM, Ls, S0, S) :- foldl(GRM, Ls, S0, S).

meta_test_tokenizer_output(In, Out) :-
  meta_test_tokenizer_output(In, Out, XIn, XOut),
  XIn == [],
  XOut == Out,
  !.
meta_test_tokenizer_output(In, _, XIn, XOut) :-
  empty_pos(L0),
  repeated_phrase(token, XOut, [L0 | In], XIn).

nwdet_ok(_:T) :- nwdet(T).
:- discontiguous(nwdet/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BEGIN TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_tokenizer_eof :-
  In = "",
  Out = [tkn(pos(0,0,0), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_ws :-
  In = "       \t  \t \r\n \n \r  \r\n ",
  Out = [tkn(pos(3, 1, 23), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_comment :-
  In = "  #   asdf sa i34 271 b3fuh \t  \t \n ",
  Out = [tkn(pos(1, 1, 35), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_special_chars :-
  In = "_,;.@()[]",
  Out = [
    tkn(pos(0,0,0), underscore),
    tkn(pos(0,1,1), comma),
    tkn(pos(0,2,2), semi),
    tkn(pos(0,3,3), dot),
    tkn(pos(0,4,4), at),
    tkn(pos(0,5,5), open_par),
    tkn(pos(0,6,6), close_par),
    tkn(pos(0,7,7), open_square),
    tkn(pos(0,8,8), close_square),
    tkn(pos(0,9,9), eof)
  ],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_single :-
  In = "'asdf qwer 1234'",
  Out = [tkn(pos(0,0,0), string("asdf qwer 1234")), tkn(pos(0,16,16), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_single_escapes :-
  In = "'asdf\\t\\b\\n\\r\\f\\'\\\"\\\\qwer'",
  Out = [tkn(pos(0,0,0), string("asdf\t\b\n\r\f\'\"\\qwer")), tkn(pos(0,26,26), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_single_escapes_uU :-
  In = "'\\uabcd\\U0010cdef'",
  Out = [tkn(pos(0,0,0), string("\xabcd\\x0010cdef\")), tkn(pos(0,18,18), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_longsingle :-
  In = "'''asdf qwer 1234'''",
  Out = [tkn(pos(0,0,0), string("asdf qwer 1234")), tkn(pos(0,20,20), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_longsingle_escapes :-
  In = "'''asdf\\t\\b\\n\\r\\f\\'\\\"\\\\qwer'''",
  Out = [tkn(pos(0,0,0), string("asdf\t\b\n\r\f\'\"\\qwer")), tkn(pos(0,30,30), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_longsingle_escapes_uU :-
  In = "'''\\uabcd\\U0010cdef'''",
  Out = [tkn(pos(0,0,0), string("\xabcd\\x0010cdef\")), tkn(pos(0,22,22), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_longsingle_specific_chars :-
  In = "'''asdf'qw''zxcvg'\r\n'lalala' b'''",
  Out = [tkn(pos(0,0,0), string("asdf'qw''zxcvg'\r\n'lalala' b")), tkn(pos(1,13,33), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_double :-
  In = "\"asdf qwer 1234\"",
  Out = [tkn(pos(0,0,0), string("asdf qwer 1234")), tkn(pos(0,16,16), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_double_escapes :-
  In = "\"asdf\\t\\b\\n\\r\\f\\'\\\"\\\\qwer\"",
  Out = [tkn(pos(0,0,0), string("asdf\t\b\n\r\f\'\"\\qwer")), tkn(pos(0,26,26), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_double_escapes_uU :-
  In = "\"\\uabcd\\U0010cdef\"",
  Out = [tkn(pos(0,0,0), string("\xabcd\\x0010cdef\")), tkn(pos(0,18,18), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_longdouble :-
  In = "\"\"\"asdf qwer 1234\"\"\"",
  Out = [tkn(pos(0,0,0), string("asdf qwer 1234")), tkn(pos(0,20,20), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_longdouble_escapes :-
  In = "\"\"\"asdf\\t\\b\\n\\r\\f\\'\\\"\\\\qwer\"\"\"",
  Out = [tkn(pos(0,0,0), string("asdf\t\b\n\r\f\'\"\\qwer")), tkn(pos(0,30,30), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_longdouble_escapes_uU :-
  In = "\"\"\"\\uabcd\\U0010cdef\"\"\"",
  Out = [tkn(pos(0,0,0), string("\xabcd\\x0010cdef\")), tkn(pos(0,22,22), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_string_longdouble_specific_chars :-
  In = "\"\"\"asdf\"qw\"\"zxcvg\"\r\n\"lalala\" b\"\"\"",
  Out = [tkn(pos(0,0,0), string("asdf\"qw\"\"zxcvg\"\r\n\"lalala\" b")), tkn(pos(1,13,33), eof)],
  meta_test_tokenizer_output(In, Out),
true.

% TODO: test string error cases

test_tokenizer_iriref :-
  In = "<asdf/qwer>",
  Out = [tkn(pos(0,0,0), iriref("asdf/qwer")), tkn(pos(0,11,11), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_iriref_empty :-
  In = "<>",
  Out = [tkn(pos(0,0,0), iriref("")), tkn(pos(0,2,2), eof)],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_iriref_escape_uU :-
  In = "<\\uabcd\\U0010cdef>",
  Out = [tkn(pos(0,0,0), iriref("\xabcd\\x0010cdef\")), tkn(pos(0,18,18), eof)],
  meta_test_tokenizer_output(In, Out),
true.

% TODO: test iriref error cases

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  END  TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_tests :- run_tests("test_").

run_tests(Prefix) :- run_tests(Prefix, test_turtle).

writen(X) :- write(X), nl.
