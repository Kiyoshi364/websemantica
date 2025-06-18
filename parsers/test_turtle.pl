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
  $(XIn == []),
  writen(xout),
  $(XOut == Out),
  !.

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
  In = "_,;.()[]^^a^",
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
    tkn(pos(0,10,10), a),
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
  In = "asdf: asd\xabcd\\x2040\f: : true false",
  Out = [
    tkn(pos(0,0,0), namespace("asdf")),
    tkn(pos(0,6,6), namespace("asd\xabcd\\x2040\f")),
    tkn(pos(0,14,16), namespace("")),
    tkn(pos(0,16,18), boolean("true")),
    tkn(pos(0,21,23), boolean("false")),
    tkn(pos(0,26,28), eof)
  ],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_sparql_prefix :-
  In = "prefix prefiX prefIx prefIX preFix preFiX preFIx preFIX prEfix prEfiX prEfIx prEfIX prEFix prEFiX prEFIx prEFIX pRefix pRefiX pRefIx pRefIX pReFix pReFiX pReFIx pReFIX pREfix pREfiX pREfIx pREfIX pREFix pREFiX pREFIx pREFIX Prefix PrefiX PrefIx PrefIX PreFix PreFiX PreFIx PreFIX PrEfix PrEfiX PrEfIx PrEfIX PrEFix PrEFiX PrEFIx PrEFIX PRefix PRefiX PRefIx PRefIX PReFix PReFiX PReFIx PReFIX PREfix PREfiX PREfIx PREfIX PREFix PREFiX PREFIx PREFIX",
  Out = [
    tkn(pos(0,0,0),sparql_prefix),
    tkn(pos(0,7,7),sparql_prefix),
    tkn(pos(0,14,14),sparql_prefix),
    tkn(pos(0,21,21),sparql_prefix),
    tkn(pos(0,28,28),sparql_prefix),
    tkn(pos(0,35,35),sparql_prefix),
    tkn(pos(0,42,42),sparql_prefix),
    tkn(pos(0,49,49),sparql_prefix),
    tkn(pos(0,56,56),sparql_prefix),
    tkn(pos(0,63,63),sparql_prefix),
    tkn(pos(0,70,70),sparql_prefix),
    tkn(pos(0,77,77),sparql_prefix),
    tkn(pos(0,84,84),sparql_prefix),
    tkn(pos(0,91,91),sparql_prefix),
    tkn(pos(0,98,98),sparql_prefix),
    tkn(pos(0,105,105),sparql_prefix),
    tkn(pos(0,112,112),sparql_prefix),
    tkn(pos(0,119,119),sparql_prefix),
    tkn(pos(0,126,126),sparql_prefix),
    tkn(pos(0,133,133),sparql_prefix),
    tkn(pos(0,140,140),sparql_prefix),
    tkn(pos(0,147,147),sparql_prefix),
    tkn(pos(0,154,154),sparql_prefix),
    tkn(pos(0,161,161),sparql_prefix),
    tkn(pos(0,168,168),sparql_prefix),
    tkn(pos(0,175,175),sparql_prefix),
    tkn(pos(0,182,182),sparql_prefix),
    tkn(pos(0,189,189),sparql_prefix),
    tkn(pos(0,196,196),sparql_prefix),
    tkn(pos(0,203,203),sparql_prefix),
    tkn(pos(0,210,210),sparql_prefix),
    tkn(pos(0,217,217),sparql_prefix),
    tkn(pos(0,224,224),sparql_prefix),
    tkn(pos(0,231,231),sparql_prefix),
    tkn(pos(0,238,238),sparql_prefix),
    tkn(pos(0,245,245),sparql_prefix),
    tkn(pos(0,252,252),sparql_prefix),
    tkn(pos(0,259,259),sparql_prefix),
    tkn(pos(0,266,266),sparql_prefix),
    tkn(pos(0,273,273),sparql_prefix),
    tkn(pos(0,280,280),sparql_prefix),
    tkn(pos(0,287,287),sparql_prefix),
    tkn(pos(0,294,294),sparql_prefix),
    tkn(pos(0,301,301),sparql_prefix),
    tkn(pos(0,308,308),sparql_prefix),
    tkn(pos(0,315,315),sparql_prefix),
    tkn(pos(0,322,322),sparql_prefix),
    tkn(pos(0,329,329),sparql_prefix),
    tkn(pos(0,336,336),sparql_prefix),
    tkn(pos(0,343,343),sparql_prefix),
    tkn(pos(0,350,350),sparql_prefix),
    tkn(pos(0,357,357),sparql_prefix),
    tkn(pos(0,364,364),sparql_prefix),
    tkn(pos(0,371,371),sparql_prefix),
    tkn(pos(0,378,378),sparql_prefix),
    tkn(pos(0,385,385),sparql_prefix),
    tkn(pos(0,392,392),sparql_prefix),
    tkn(pos(0,399,399),sparql_prefix),
    tkn(pos(0,406,406),sparql_prefix),
    tkn(pos(0,413,413),sparql_prefix),
    tkn(pos(0,420,420),sparql_prefix),
    tkn(pos(0,427,427),sparql_prefix),
    tkn(pos(0,434,434),sparql_prefix),
    tkn(pos(0,441,441),sparql_prefix),
    tkn(pos(0,447,447),eof)
  ],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_sparql_base :-
  In = "base basE baSe baSE bAse bAsE bASe bASE Base BasE BaSe BaSE BAse BAsE BASe BASE",
  Out = [
    tkn(pos(0,0,0), sparql_base),
    tkn(pos(0,5,5), sparql_base),
    tkn(pos(0,10,10), sparql_base),
    tkn(pos(0,15,15), sparql_base),
    tkn(pos(0,20,20), sparql_base),
    tkn(pos(0,25,25), sparql_base),
    tkn(pos(0,30,30), sparql_base),
    tkn(pos(0,35,35), sparql_base),
    tkn(pos(0,40,40), sparql_base),
    tkn(pos(0,45,45), sparql_base),
    tkn(pos(0,50,50), sparql_base),
    tkn(pos(0,55,55), sparql_base),
    tkn(pos(0,60,60), sparql_base),
    tkn(pos(0,65,65), sparql_base),
    tkn(pos(0,70,70), sparql_base),
    tkn(pos(0,75,75), sparql_base),
    tkn(pos(0,79,79),eof)
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

test_parser_triple_simple :-
  In = "<sub> <verb> <obj> . <sub1> <verb1> <obj1> .",
  Ts = [
    t("sub", "verb", "obj"),
    t("sub1", "verb1", "obj1")
  ],
  S = ps_b_b([], [], 0),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_triple_simple_semi_comma :-
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

test_parser_triple_simple_strings :-
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

test_parser_triple_simple_literals :-
  In = "<sub> <verb> <obj>, 'asdf'@en, '1234'^^<foo>, 1234, -10.2, +3.4e-7, false, true .",
  Ts = [
    t("sub", "verb", "obj"),
    t("sub", "verb", literal(LangStrTy, lang_string("en", "asdf"))),
    t("sub", "verb", literal("foo", "1234")),
    t("sub", "verb", literal(IntegerTy, "1234")),
    t("sub", "verb", literal(DecimalTy, "-10.2")),
    t("sub", "verb", literal(DoubleTy, "+3.4e-7")),
    t("sub", "verb", literal(BooleanTy, "false")),
    t("sub", "verb", literal(BooleanTy, "true"))
  ],
  S = ps_b_b([], [], 0),
  tag_type(lang_string, LangStrTy),
  tag_type(integer, IntegerTy),
  tag_type(decimal, DecimalTy),
  tag_type(double, DoubleTy),
  tag_type(boolean, BooleanTy),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_prefix :-
  In = "@prefix : <http://prefix.com/> . PREFIX ex: <http://example.com/> ex:sub :verb :obj .",
  Ts = [
    t("http://example.com/sub", "http://prefix.com/verb", "http://prefix.com/obj")
  ],
  S = ps_b_b(["ex"-"http://example.com/", []-"http://prefix.com/"], [], 0),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_prefix_override :-
  In = "@prefix : <http://prefix.com/> . :sub :verb :obj . @prefix : <http://prefix1.com/> . :sub :verb :obj .",
  Ts = [
    t("http://prefix.com/sub", "http://prefix.com/verb", "http://prefix.com/obj"),
    t("http://prefix1.com/sub", "http://prefix1.com/verb", "http://prefix1.com/obj")
  ],
  S = ps_b_b([[]-"http://prefix1.com/"], [], 0),
  meta_test_parser_output(In, Ts, S),
true.

%%%%%%%%%%%%%%%  END  Parser %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  END  TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_tests :- run_tests("test_").

run_tests(Prefix) :- run_tests(Prefix, test_turtle).

writen(X) :- write(X), nl.
