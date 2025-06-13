:- module(test_turtle, [
  run_tests/0, run_tests/1
]).

:- use_module(turtle, [
  token//1,
  empty_state/1, parse//2,
  tag_type/2
]).

:- use_module(library(dcgs), [phrase/3]).
:- use_module(library(lists), [
  foldl/4
]).

:- use_module(library(debug)).

:- use_module(linecol, [empty_pos/1]).

:- use_module(libtest).

repeated_phrase(GRM, Ls, S0, S) :- foldl(GRM, Ls, S0, S).

nwdet_ok(_:T) :- nwdet(T).
:- discontiguous(nwdet/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BEGIN TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% META  Tokenizer %%%%%%%%%%%%%%%

meta_test_tokenizer_output(In, Out) :-
  meta_test_tokenizer_output(In, Out, XIn, XOut),
  XIn == [],
  XOut == Out,
  !.
meta_test_tokenizer_output(In, _, XIn, XOut) :-
  empty_pos(L0),
  repeated_phrase(token, XOut, [L0 | In], XIn).

meta_test_tokenizer_output_(In, Out) :-
  meta_test_tokenizer_output(In, Out, XIn, XOut),
  writen(xin),
  writen(XIn),
  writen(xout),
  writen(XOut),
  false.

%%%%%%%%%%%%%%% BEGIN Tokenizer %%%%%%%%%%%%%%%

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
  In = "_,;.()[]^^ ^",
  Out = [
    tkn(pos(0,0,0), underscore),
    tkn(pos(0,1,1), comma),
    tkn(pos(0,2,2), semi),
    tkn(pos(0,3,3), dot),
    tkn(pos(0,4,4), open_par),
    tkn(pos(0,5,5), close_par),
    tkn(pos(0,6,6), open_square),
    tkn(pos(0,7,7), close_square),
    tkn(pos(0,8,8), double_carrot),
    tkn(pos(0,11,11), carrot),
    tkn(pos(0,12,12), eof)
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

test_tokenizer_numbers :-
  In = "01234 +5 -6 7.8 9.e+10 1e-2 2e4",
  Out = [
    tkn(pos(0,0,0), number(integer, "01234")),
    tkn(pos(0,6,6), number(integer, "+5")),
    tkn(pos(0,9,9), number(integer, "-6")),
    tkn(pos(0,12,12), number(decimal, "7.8")),
    tkn(pos(0,16,16), number(double, "9.e+10")),
    tkn(pos(0,23,23), number(double, "1e-2")),
    tkn(pos(0,28,28), number(double, "2e4")),
    tkn(pos(0,31,31), eof)
  ],
  meta_test_tokenizer_output(In, Out),
true.

% TODO: test number error cases

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

test_tokenizer_namespace :-
  In = "asdf: asd\xabcd\\x2040\f: :",
  Out = [
    tkn(pos(0,0,0), namespace("asdf")),
    tkn(pos(0,6,6), namespace("asd\xabcd\\x2040\f")),
    tkn(pos(0,14,16), namespace("")),
    tkn(pos(0,15,17), eof)
  ],
  meta_test_tokenizer_output(In, Out),
true.

% TODO: test namespace error cases

test_tokenizer_prefixed :-
  In = "asdf:qwer asd\xabcd\\x2040\f:%20\\+\\@.com :1234\\(\\)",
  Out = [
    tkn(pos(0,0,0), prefixed("asdf", "qwer")),
    tkn(pos(0,10,10), prefixed("asd\xabcd\\x2040\f", " +@.com")),
    tkn(pos(0,29,31), prefixed("", "1234()")),
    tkn(pos(0,38,40), eof)
  ],
  meta_test_tokenizer_output(In, Out),
true.

% TODO: test namespace error cases

test_tokenizer_langtag :-
  In = "@pt-br@en @asd-1234f-xxx @prefix @base",
  Out = [
    tkn(pos(0,0,0), langtag("pt-br")),
    tkn(pos(0,6,6), langtag("en")),
    tkn(pos(0,10,10), langtag("asd-1234f-xxx")),
    tkn(pos(0,25,25), langtag("prefix")),
    tkn(pos(0,33,33), langtag("base")),
    tkn(pos(0,38,38), eof)
  ],
  meta_test_tokenizer_output(In, Out),
true.

% TODO: test langtag error cases

%%%%%%%%%%%%%%%  END  Tokenizer %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% META  Parser %%%%%%%%%%%%%%%

meta_test_parser_output(In, Ts, S) :-
  meta_test_parser_output(In, Ts, S, XIn, XTs, XS),
  XIn == [],
  XTs == Ts,
  XS == S,
  !.
meta_test_parser_output(In, _, _, XIn, XTs, XS) :-
  empty_pos(L0),
  phrase(parse(XTs, XS), [L0 | In], XIn).

meta_test_parser_output_(In, Ts, S) :-
  meta_test_parser_output(In, Ts, S, XIn, XTs, XS),
  $(XIn == []),
  $(XTs == Ts),
  $(XS == S),
  !.

%%%%%%%%%%%%%%% BEGIN Parser %%%%%%%%%%%%%%%

test_triple_simple :-
  In = "<sub> <verb> <obj> . <sub1> <verb1> <obj1> .",
  Ts = [
    t("sub", "verb", "obj"),
    t("sub1", "verb1", "obj1")
  ],
  S = ps_b_b([], [], 0),
  meta_test_parser_output(In, Ts, S),
true.

test_triple_simple_semi_comma :-
  In = "<sub> <verb> <obj>, <obj1>; <verb1> <obj2>, <obj3>, <obj4> .",
  Ts = [
    t("sub", "verb", "obj"),
    t("sub", "verb", "obj1"),
    t("sub", "verb1", "obj2"),
    t("sub", "verb1", "obj3"),
    t("sub", "verb1", "obj4")
  ],
  S = ps_b_b([], [], 0),
  meta_test_parser_output(In, Ts, S),
true.

test_triple_simple_strings :-
  In = "<sub> <verb> <obj>, 'asdf', \"qwer\", '''wiebf\nlelele\n\nqwe'''.",
  Ts = [
    t("sub", "verb", "obj"),
    t("sub", "verb", literal(StringTy, "asdf")),
    t("sub", "verb", literal(StringTy, "qwer")),
    t("sub", "verb", literal(StringTy, "wiebf\nlelele\n\nqwe"))
  ],
  S = ps_b_b([], [], 0),
  tag_type(string, StringTy),
  meta_test_parser_output(In, Ts, S),
true.

% TODO: uncomment booleans
test_triple_simple_literals :-
  In = "<sub> <verb> <obj>, 'asdf'@en, '1234'^^<foo>, 1234, -10.2, +3.4e-7 .", % ", false, true .",
  Ts = [
    t("sub", "verb", "obj"),
    t("sub", "verb", literal(LangStrTy, lang_string("en", "asdf"))),
    t("sub", "verb", literal("foo", "1234")),
    t("sub", "verb", literal(IntegerTy, "1234")),
    t("sub", "verb", literal(DecimalTy, "-10.2")),
    t("sub", "verb", literal(DoubleTy, "+3.4e-7"))
    % t("sub", "verb", literal(BooleanTy, "false")),
    % t("sub", "verb", literal(BooleanTy, "true"))
  ],
  S = ps_b_b([], [], 0),
  tag_type(lang_string, LangStrTy),
  tag_type(integer, IntegerTy),
  tag_type(decimal, DecimalTy),
  tag_type(double, DoubleTy),
  % tag_type(boolean, BooleanTy),
  meta_test_parser_output(In, Ts, S),
true.

%%%%%%%%%%%%%%%  END  Parser %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  END  TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_tests :- run_tests("test_").

run_tests(Prefix) :- run_tests(Prefix, test_turtle).

writen(X) :- write(X), nl.
