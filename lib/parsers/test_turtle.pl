:- module(test_turtle, [
  run_tests/0, run_tests/1
]).

:- use_module(turtle, [
  empty_pos/1, token//1,
  empty_state/1, parse//2,
  tag_type/2, tag_iri/2
]).

:- use_module(library(dcgs), [phrase/3]).
:- use_module(library(lists), [
  foldl/4
]).

:- use_module(library(debug)).

:- use_module('../libtest').

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
  In = ",;.()[ ][^^]a^",
  Out = [
    tkn(pos(0,0,0), comma),
    tkn(pos(0,1,1), semi),
    tkn(pos(0,2,2), dot),
    tkn(pos(0,3,3), open_par),
    tkn(pos(0,4,4), close_par),
    tkn(pos(0,5,5), anon),
    tkn(pos(0,8,8), open_square),
    tkn(pos(0,9,9), double_carrot),
    tkn(pos(0,11,11), close_square),
    tkn(pos(0,12,12), a),
    tkn(pos(0,13,13), carrot),
    tkn(pos(0,14,14), eof)
  ],
  meta_test_tokenizer_output(In, Out),
true.

test_tokenizer_blanknodelabel :-
  In = "_:a _:1.2.",
  Out = [
    tkn(pos(0,0,0), blank_node("_:a")),
    tkn(pos(0,4,4), blank_node("_:1.2")),
    tkn(pos(0,9,9), dot),
    tkn(pos(0,10,10), eof)
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

test_tokenizer_localname :-
  In = "ex:a123 ex:qwer_zxcv ex:a%20.com ex:b.",
  Out = [
    tkn(pos(0,0,0), prefixed("ex", "a123")),
    tkn(pos(0,8,8), prefixed("ex", "qwer_zxcv")),
    tkn(pos(0,21,21), prefixed("ex", "a .com")),
    tkn(pos(0,33,33), prefixed("ex", "b")),
    tkn(pos(0,37,37), dot),
    tkn(pos(0,38,38), eof)
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
  phrase(parse(XTs, XS), In, XIn).

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
    t(iri(sub), iri(verb), iri(obj)),
    t(iri(sub1), iri(verb1), iri(obj1))
  ],
  S = ps_b_b([], [], 0),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_triple_simple_semi_comma :-
  In = "<sub> <verb> <obj>, <obj1>; <verb1> <obj2>, <obj3>, <obj4> .",
  Ts = [
    t(iri(sub), iri(verb), iri(obj)),
    t(iri(sub), iri(verb), iri(obj1)),
    t(iri(sub), iri(verb1), iri(obj2)),
    t(iri(sub), iri(verb1), iri(obj3)),
    t(iri(sub), iri(verb1), iri(obj4))
  ],
  S = ps_b_b([], [], 0),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_triple_simple_strings :-
  In = "<sub> <verb> <obj>, 'asdf', \"qwer\", '''wiebf\nlelele\n\nqwe'''.",
  Ts = [
    t(iri(sub), iri(verb), iri(obj)),
    t(iri(sub), iri(verb), literal(StringTy, "asdf")),
    t(iri(sub), iri(verb), literal(StringTy, "qwer")),
    t(iri(sub), iri(verb), literal(StringTy, "wiebf\nlelele\n\nqwe"))
  ],
  S = ps_b_b([], [], 0),
  tag_iri(string, StringTy),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_triple_simple_literals :-
  In = "<sub> <verb> <obj>, 'asdf'@en, '1234'^^<foo>, 1234, -10.2, +3.4e-7, false, true .",
  Ts = [
    t(iri(sub), iri(verb), iri(obj)),
    t(iri(sub), iri(verb), literal(LangStrTy, @("asdf", "en"))),
    t(iri(sub), iri(verb), literal(iri(foo), "1234")),
    t(iri(sub), iri(verb), literal(IntegerTy, "1234")),
    t(iri(sub), iri(verb), literal(DecimalTy, "-10.2")),
    t(iri(sub), iri(verb), literal(DoubleTy, "+3.4e-7")),
    t(iri(sub), iri(verb), literal(BooleanTy, "false")),
    t(iri(sub), iri(verb), literal(BooleanTy, "true"))
  ],
  S = ps_b_b([], [], 0),
  tag_iri(lang_string, LangStrTy),
  tag_iri(integer, IntegerTy),
  tag_iri(decimal, DecimalTy),
  tag_iri(double, DoubleTy),
  tag_iri(boolean, BooleanTy),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_prefix :-
  In = "@prefix : <http://prefix.com/> . PREFIX ex: <http://example.com/> ex:sub :verb :obj .",
  Ts = [
    t(iri('http://example.com/sub'), iri('http://prefix.com/verb'), iri('http://prefix.com/obj'))
  ],
  S = ps_b_b(["ex"-"http://example.com/", []-"http://prefix.com/"], [], 0),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_prefix_override :-
  In = "@prefix : <http://prefix.com/> . :sub :verb :obj . @prefix : <http://prefix1.com/> . :sub :verb :obj .",
  Ts = [
    t(iri('http://prefix.com/sub'), iri('http://prefix.com/verb'), iri('http://prefix.com/obj')),
    t(iri('http://prefix1.com/sub'), iri('http://prefix1.com/verb'), iri('http://prefix1.com/obj'))
  ],
  S = ps_b_b([[]-"http://prefix1.com/"], [], 0),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_base :-
  In = "@base <http://base.com/> . <sub> <verb> <obj> .",
  Ts = [
    t(iri('http://base.com/sub'), iri('http://base.com/verb'), iri('http://base.com/obj'))
  ],
  S = ps_b_b([], "http://base.com/", 0),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_base_override :-
  In = "@base <http://base.com/> . <sub> <verb> <obj> . BASE <http://base1.com/> <sub> <verb> <obj> .",
  Ts = [
    t(iri('http://base.com/sub'), iri('http://base.com/verb'), iri('http://base.com/obj')),
    t(iri('http://base1.com/sub'), iri('http://base1.com/verb'), iri('http://base1.com/obj'))
  ],
  S = ps_b_b([], "http://base1.com/", 0),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_blanknode :-
  In = "[ <verb1> <obj1> ]. [ <verb2> <obj2> ] <verb3> <obj3> ; <verb4> [ <verb5> <obj5> ; <verb6> _:asdf ], [ ] .",
  Ts = [
    t(blank(unlabeled, 0), iri('verb1'), iri('obj1')),
    t(blank(unlabeled, 1), iri('verb2'), iri('obj2')),
    t(blank(unlabeled, 1), iri('verb3'), iri('obj3')),
    t(blank(unlabeled, 2), iri('verb5'), iri('obj5')),
    t(blank(unlabeled, 2), iri('verb6'), blank(labeled, '_:asdf')),
    t(blank(unlabeled, 1), iri('verb4'), blank(unlabeled, 2)),
    t(blank(unlabeled, 1), iri('verb4'), blank(unlabeled, 3))
  ],
  S = ps_b_b([], "", 4),
  meta_test_parser_output(In, Ts, S),
true.

test_parser_collection :-
  In = "<sub> <verb> ( 1234 'Hello'@en 'asdf' ) .",
  Ts = [
    t(blank(unlabeled, 0), First, literal(IntegerTy, "1234")),
    t(blank(unlabeled, 0), Rest, blank(unlabeled, 1)),
    t(blank(unlabeled, 1), First, literal(LangStrTy, @("Hello", "en"))),
    t(blank(unlabeled, 1), Rest, blank(unlabeled, 2)),
    t(blank(unlabeled, 2), First, literal(StringTy, "asdf")),
    t(blank(unlabeled, 2), Rest, Nil),
    t(iri('sub'), iri('verb'), blank(unlabeled, 0))
  ],
  S = ps_b_b([], "", 3),
  tag_iri(first, First),
  tag_iri(rest, Rest),
  tag_iri(nil, Nil),
  tag_iri(integer, IntegerTy),
  tag_iri(lang_string, LangStrTy),
  tag_iri(string, StringTy),
  meta_test_parser_output(In, Ts, S),
true.

%%%%%%%%%%%%%%% BEGIN Parser Spec Examples %%%%%%%%%%%%%%%

/* Spec is available at: https://www.w3.org/TR/turtle/ */

test_spec_example1 :-
  In = "@base <http://example.org/> .\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n@prefix rel: <http://www.perceive.net/schemas/relationship/> .\n<#green-goblin>\n    rel:enemyOf <#spiderman> ;\n    a foaf:Person ;    # in the context of the Marvel universe\n    foaf:name \"Green Goblin\" .\n<#spiderman>\n    rel:enemyOf <#green-goblin> ;\n    a foaf:Person ;\n    foaf:name \"Spiderman\", \"Человек-паук\"@ru .",
  Ts = [
    t(iri('http://example.org/#green-goblin'), iri('http://www.perceive.net/schemas/relationship/enemyOf'), iri('http://example.org/#spiderman')),
    t(iri('http://example.org/#green-goblin'), iri(Type), iri('http://xmlns.com/foaf/0.1/Person')),
    t(iri('http://example.org/#green-goblin'), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Green Goblin")),
    t(iri('http://example.org/#spiderman'), iri('http://www.perceive.net/schemas/relationship/enemyOf'), iri('http://example.org/#green-goblin')),
    t(iri('http://example.org/#spiderman'), iri(Type), iri('http://xmlns.com/foaf/0.1/Person')),
    t(iri('http://example.org/#spiderman'), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Spiderman")),
    t(iri('http://example.org/#spiderman'), iri('http://xmlns.com/foaf/0.1/name'), literal(LangStrTy, @("Человек-паук", "ru")))
  ],
  S = ps_b_b([
    "rel"-"http://www.perceive.net/schemas/relationship/",
    "foaf"-"http://xmlns.com/foaf/0.1/",
    "rdfs"-"http://www.w3.org/2000/01/rdf-schema#",
    "rdf"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    ], "http://example.org/", 0),
  tag_type(a, Type),
  tag_iri(string, StringTy),
  tag_iri(lang_string, LangStrTy),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example2 :-
  In = "<http://example.org/#spiderman> <http://www.perceive.net/schemas/relationship/enemyOf> <http://example.org/#green-goblin> .",
  Ts = [
    t(iri('http://example.org/#spiderman'), iri('http://www.perceive.net/schemas/relationship/enemyOf'), iri('http://example.org/#green-goblin'))
  ],
  S = ps_b_b([], "", 0),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example3_4 :-
  In3 = "<http://example.org/#spiderman> <http://www.perceive.net/schemas/relationship/enemyOf> <http://example.org/#green-goblin> ;\n\t\t\t\t<http://xmlns.com/foaf/0.1/name> \"Spiderman\" .",
  In4 = "<http://example.org/#spiderman> <http://www.perceive.net/schemas/relationship/enemyOf> <http://example.org/#green-goblin> .\n<http://example.org/#spiderman> <http://xmlns.com/foaf/0.1/name> \"Spiderman\" .",
  Ts = [
    t(iri('http://example.org/#spiderman'), iri('http://www.perceive.net/schemas/relationship/enemyOf'), iri('http://example.org/#green-goblin')),
    t(iri('http://example.org/#spiderman'), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Spiderman"))
  ],
  S = ps_b_b([], "", 0),
  tag_iri(string, StringTy),
  meta_test_parser_output(In3, Ts, S),
  meta_test_parser_output(In4, Ts, S),
true.

test_spec_example5_6 :-
  In5 = "<http://example.org/#spiderman> <http://xmlns.com/foaf/0.1/name> \"Spiderman\", \"Человек-паук\"@ru .",
  In6 = "<http://example.org/#spiderman> <http://xmlns.com/foaf/0.1/name> \"Spiderman\" .\n<http://example.org/#spiderman> <http://xmlns.com/foaf/0.1/name> \"Человек-паук\"@ru .",
  Ts = [
    t(iri('http://example.org/#spiderman'), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Spiderman")),
    t(iri('http://example.org/#spiderman'), iri('http://xmlns.com/foaf/0.1/name'), literal(LangStrTy, @("Человек-паук", "ru")))
  ],
  S = ps_b_b([], "", 0),
  tag_iri(string, StringTy),
  tag_iri(lang_string, LangStrTy),
  meta_test_parser_output(In5, Ts, S),
  meta_test_parser_output(In6, Ts, S),
true.

test_spec_example7_8 :-
  In7 = "@prefix somePrefix: <http://www.perceive.net/schemas/relationship/> .\n<http://example.org/#green-goblin> somePrefix:enemyOf <http://example.org/#spiderman> .",
  In8 = "PREFIX somePrefix: <http://www.perceive.net/schemas/relationship/>\n<http://example.org/#green-goblin> somePrefix:enemyOf <http://example.org/#spiderman> .",
  Ts = [
    t(iri('http://example.org/#green-goblin'), iri('http://www.perceive.net/schemas/relationship/enemyOf'), iri('http://example.org/#spiderman'))
  ],
  S = ps_b_b(["somePrefix"-"http://www.perceive.net/schemas/relationship/"], "", 0),
  meta_test_parser_output(In7, Ts, S),
  meta_test_parser_output(In8, Ts, S),
true.

test_spec_example9 :-
  In = "# A triple with all absolute IRIs\n<http://one.example/subject1> <http://one.example/predicate1> <http://one.example/object1> .\n@base <http://one.example/> .\n<subject2> <predicate2> <object2> .     # relative IRIs, e.g. http://one.example/subject2\nBASE <http://one.example/>\n<subject2> <predicate2> <object2> .     # relative IRIs, e.g. http://one.example/subject2\n@prefix p: <http://two.example/> .\np:subject3 p:predicate3 p:object3 .     # prefixed name, e.g. http://two.example/subject3\nPREFIX p: <http://two.example/>\np:subject3 p:predicate3 p:object3 .     # prefixed name, e.g. http://two.example/subject3\n@prefix p: <path/> .                    # prefix p: now stands for http://one.example/path/\np:subject4 p:predicate4 p:object4 .     # prefixed name, e.g. http://one.example/path/subject4\n@prefix : <http://another.example/> .    # empty prefix\n:subject5 :predicate5 :object5 .        # prefixed name, e.g. http://another.example/subject5\n:subject6 a :subject7 .                 # same as :subject6 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> :subject7 .\n<http://伝言.example/?user=أكرم&amp;channel=R%26D> a :subject8 . # a multi-script subject IRI .",
  % TODO: This needs fixing when we get a "smarter checking" on `append_base/2`
  Ts = [
    t(iri('http://one.example/subject1'), iri('http://one.example/predicate1'), iri('http://one.example/object1')),
    t(iri('http://one.example/subject2'), iri('http://one.example/predicate2'), iri('http://one.example/object2')),
    t(iri('http://one.example/subject2'), iri('http://one.example/predicate2'), iri('http://one.example/object2')),
    t(iri('http://two.example/subject3'), iri('http://two.example/predicate3'), iri('http://two.example/object3')),
    t(iri('http://two.example/subject3'), iri('http://two.example/predicate3'), iri('http://two.example/object3')),
    % t(iri('http://one.example/path/subject4'), iri('http://one.example/path/predicate4'), iri('http://one.example/path/object4')),
    t(iri('path/subject4'), iri('path/predicate4'), iri('path/object4')),
    t(iri('http://another.example/subject5'), iri('http://another.example/predicate5'), iri('http://another.example/object5')),
    t(iri('http://another.example/subject6'), iri(Type), iri('http://another.example/subject7')),
    % t(iri('http://伝言.example/?user=أكرم&amp;channel=R%26D'), iri(Type), iri('http://another.example/subject8'))
    t(iri('http://one.example/http://伝言.example/?user=أكرم&amp;channel=R%26D'), iri(Type), iri('http://another.example/subject8'))
  ],
  S = ps_b_b([
    ""-"http://another.example/",
    % "p"-"http://one.example/path"
    "p"-"path/"
  ], "http://one.example/", 0),
  tag_type(a, Type),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example10 :-
  In = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n<http://example.org/#green-goblin> foaf:name \"Green Goblin\" .\n<http://example.org/#spiderman> foaf:name \"Spiderman\" .",
  Ts = [
    t(iri('http://example.org/#green-goblin'), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Green Goblin")),
    t(iri('http://example.org/#spiderman'), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Spiderman"))
  ],
  S = ps_b_b([
    "foaf"-"http://xmlns.com/foaf/0.1/"
  ], "", 0),
  tag_iri(string, StringTy),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example11 :-
  In = "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix show: <http://example.org/vocab/show/> .\n@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\nshow:218 rdfs:label \"That Seventies Show\"^^xsd:string .            # literal with XML Schema string datatype\nshow:218 rdfs:label \"That Seventies Show\"^^<http://www.w3.org/2001/XMLSchema#string> . # same as above\nshow:218 rdfs:label \"That Seventies Show\" .                                            # same again\nshow:218 show:localName \"That Seventies Show\"@en .                 # literal with a language tag\nshow:218 show:localName 'Cette Série des Années Soixante-dix'@fr . # literal delimited by single quote\nshow:218 show:localName \"Cette Série des Années Septante\"@fr-be .  # literal with a region subtag\nshow:218 show:blurb '''This is a multi-line                        # literal with embedded new lines and quotes\nliteral with many quotes (\"\"\"\"\")\nand up to two sequential apostrophes ('').''' .",
  Ts = [
    t(iri('http://example.org/vocab/show/218'), iri('http://www.w3.org/2000/01/rdf-schema#label'), literal(StringTy, "That Seventies Show")),
    t(iri('http://example.org/vocab/show/218'), iri('http://www.w3.org/2000/01/rdf-schema#label'), literal(StringTy, "That Seventies Show")),
    t(iri('http://example.org/vocab/show/218'), iri('http://www.w3.org/2000/01/rdf-schema#label'), literal(StringTy, "That Seventies Show")),
    t(iri('http://example.org/vocab/show/218'), iri('http://example.org/vocab/show/localName'), literal(LangStrTy, @("That Seventies Show", "en"))),
    t(iri('http://example.org/vocab/show/218'), iri('http://example.org/vocab/show/localName'), literal(LangStrTy, @("Cette Série des Années Soixante-dix", "fr"))),
    t(iri('http://example.org/vocab/show/218'), iri('http://example.org/vocab/show/localName'), literal(LangStrTy, @("Cette Série des Années Septante", "fr-be"))),
    t(iri('http://example.org/vocab/show/218'), iri('http://example.org/vocab/show/blurb'), literal(StringTy, "This is a multi-line                        # literal with embedded new lines and quotes\nliteral with many quotes (\"\"\"\"\")\nand up to two sequential apostrophes ('')."))
  ],
  S = ps_b_b([
    "xsd"-"http://www.w3.org/2001/XMLSchema#",
    "show"-"http://example.org/vocab/show/",
    "rdfs"-"http://www.w3.org/2000/01/rdf-schema#"
  ], "", 0),
  tag_iri(string, StringTy),
  tag_iri(lang_string, LangStrTy),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example12 :-
  In = "@prefix : <http://example.org/elements> .                                                                              \n<http://en.wikipedia.org/wiki/Helium>                                                                                  \n    :atomicNumber 2 ;               # xsd:integer                                                                      \n    :atomicMass 4.002602 ;          # xsd:decimal                                                                      \n    :specificGravity 1.663E-4 .     # xsd:double  ",
  Ts = [
    t(iri('http://en.wikipedia.org/wiki/Helium'), iri('http://example.org/elementsatomicNumber'), literal(IntegerTy, "2")),
    t(iri('http://en.wikipedia.org/wiki/Helium'), iri('http://example.org/elementsatomicMass'), literal(DecimalTy, "4.002602")),
    t(iri('http://en.wikipedia.org/wiki/Helium'), iri('http://example.org/elementsspecificGravity'), literal(DoubleTy, "1.663E-4"))
  ],
  S = ps_b_b([""-"http://example.org/elements"], "", 0),
  tag_iri(integer, IntegerTy),
  tag_iri(decimal, DecimalTy),
  tag_iri(double, DoubleTy),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example13 :-
  In = "@prefix : <http://example.org/stats> .\n<http://somecountry.example/census2007>\n    :isLandlocked false .           # xsd:boolean",
  Ts = [
    t(iri('http://somecountry.example/census2007'), iri('http://example.org/statsisLandlocked'), literal(BooleanTy, "false"))
  ],
  S = ps_b_b([""-"http://example.org/stats"], "", 0),
  tag_iri(boolean, BooleanTy),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example14 :-
  In = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n_:alice foaf:knows _:bob .\n_:bob foaf:knows _:alice .",
  Ts = [
    t(blank(labeled, '_:alice'), iri('http://xmlns.com/foaf/0.1/knows'), blank(labeled, '_:bob')),
    t(blank(labeled, '_:bob'), iri('http://xmlns.com/foaf/0.1/knows'), blank(labeled, '_:alice'))
  ],
  S = ps_b_b(["foaf"-"http://xmlns.com/foaf/0.1/"], "", 0),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example15 :-
  In = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n# Someone knows someone else, who has the name \"Bob\".\n[] foaf:knows [ foaf:name \"Bob\" ] .",
  Ts = [
    t(blank(unlabeled, 1), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Bob")),
    t(blank(unlabeled, 0), iri('http://xmlns.com/foaf/0.1/knows'), blank(unlabeled, 1))
  ],
  S = ps_b_b(["foaf"-"http://xmlns.com/foaf/0.1/"], "", 2),
  tag_iri(string, StringTy),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example16_17 :-
  In16 = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n[ foaf:name \"Alice\" ] foaf:knows [\n    foaf:name \"Bob\" ;\n    foaf:knows [\n        foaf:name \"Eve\" ] ;\n    foaf:mbox <bob@example.com> ] .",
  In17 = "_:a <http://xmlns.com/foaf/0.1/name> \"Alice\" .\n_:a <http://xmlns.com/foaf/0.1/knows> _:b .\n_:b <http://xmlns.com/foaf/0.1/name> \"Bob\" .\n_:b <http://xmlns.com/foaf/0.1/knows> _:c .\n_:c <http://xmlns.com/foaf/0.1/name> \"Eve\" .\n_:b <http://xmlns.com/foaf/0.1/mbox> <bob@example.com> .",
  Ts16 = [
    t(blank(unlabeled, 0), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Alice")),
    t(blank(unlabeled, 1), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Bob")),
    t(blank(unlabeled, 2), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Eve")),
    t(blank(unlabeled, 1), iri('http://xmlns.com/foaf/0.1/knows'), blank(unlabeled, 2)),
    t(blank(unlabeled, 1), iri('http://xmlns.com/foaf/0.1/mbox'), iri('bob@example.com')),
    t(blank(unlabeled, 0), iri('http://xmlns.com/foaf/0.1/knows'), blank(unlabeled, 1))
  ],
  Ts17 = [
    t(blank(labeled, '_:a'), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Alice")),
    t(blank(labeled, '_:a'), iri('http://xmlns.com/foaf/0.1/knows'), blank(labeled, '_:b')),
    t(blank(labeled, '_:b'), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Bob")),
    t(blank(labeled, '_:b'), iri('http://xmlns.com/foaf/0.1/knows'), blank(labeled, '_:c')),
    t(blank(labeled, '_:c'), iri('http://xmlns.com/foaf/0.1/name'), literal(StringTy, "Eve")),
    t(blank(labeled, '_:b'), iri('http://xmlns.com/foaf/0.1/mbox'), iri('bob@example.com'))
  ],
  S16 = ps_b_b(["foaf"-"http://xmlns.com/foaf/0.1/"], "", 3),
  S17 = ps_b_b([], "", 0),
  tag_iri(string, StringTy),
  meta_test_parser_output(In16, Ts16, S16),
  meta_test_parser_output(In17, Ts17, S17),
true.

test_spec_example18 :-
  In = "@prefix : <http://example.org/foo> .\n# the object of this triple is the RDF collection blank node\n:subject :predicate ( :a :b :c ) .\n# an empty collection value - rdf:nil\n:subject :predicate2 () .",
  Ts = [
    t(blank(unlabeled, 0), First, iri('http://example.org/fooa')),
    t(blank(unlabeled, 0), Rest, blank(unlabeled, 1)),
    t(blank(unlabeled, 1), First, iri('http://example.org/foob')),
    t(blank(unlabeled, 1), Rest, blank(unlabeled, 2)),
    t(blank(unlabeled, 2), First, iri('http://example.org/fooc')),
    t(blank(unlabeled, 2), Rest, Nil),
    t(iri('http://example.org/foosubject'), iri('http://example.org/foopredicate'), blank(unlabeled, 0)),
    t(iri('http://example.org/foosubject'), iri('http://example.org/foopredicate2'), Nil)
  ],
  S = ps_b_b([""-"http://example.org/foo"], "", 3),
  tag_iri(first, First),
  tag_iri(rest, Rest),
  tag_iri(nil, Nil),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example19 :-
  In = "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix dc: <http://purl.org/dc/elements/1.1/> .\n@prefix ex: <http://example.org/stuff/1.0/> .\n<http://www.w3.org/TR/rdf-syntax-grammar>\n  dc:title \"RDF/XML Syntax Specification (Revised)\" ;\n  ex:editor [\n    ex:fullname \"Dave Beckett\";\n    ex:homePage <http://purl.org/net/dajobe/>\n  ] .",
  Ts = [
    t(iri('http://www.w3.org/TR/rdf-syntax-grammar'), iri('http://purl.org/dc/elements/1.1/title'), literal(StringTy, "RDF/XML Syntax Specification (Revised)")),
    t(blank(unlabeled, 0), iri('http://example.org/stuff/1.0/fullname'), literal(StringTy, "Dave Beckett")),
    t(blank(unlabeled, 0), iri('http://example.org/stuff/1.0/homePage'), iri('http://purl.org/net/dajobe/')),
    t(iri('http://www.w3.org/TR/rdf-syntax-grammar'), iri('http://example.org/stuff/1.0/editor'), blank(unlabeled, 0))
  ],
  S = ps_b_b([
    "ex"-"http://example.org/stuff/1.0/",
    "dc"-"http://purl.org/dc/elements/1.1/",
    "rdf"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  ], "", 1),
  tag_iri(string, StringTy),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example20_21 :-
  In20 = "PREFIX : <http://example.org/stuff/1.0/>\n:a :b ( \"apple\" \"banana\" ) .",
  In21 = "@prefix : <http://example.org/stuff/1.0/> .\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n:a :b\n  [ rdf:first \"apple\";\n    rdf:rest [ rdf:first \"banana\";\n               rdf:rest rdf:nil ]\n  ] .",
  Ts20 = [
    t(blank(unlabeled, 0), First, literal(StringTy, "apple")),
    t(blank(unlabeled, 0), Rest, blank(unlabeled, 1)),
    t(blank(unlabeled, 1), First, literal(StringTy, "banana")),
    t(blank(unlabeled, 1), Rest, Nil),
    t(iri('http://example.org/stuff/1.0/a'), iri('http://example.org/stuff/1.0/b'), blank(unlabeled, 0))
  ],
  Ts21 = [
    t(blank(unlabeled, 0), First, literal(StringTy, "apple")),
    t(blank(unlabeled, 1), First, literal(StringTy, "banana")),
    t(blank(unlabeled, 1), Rest, Nil),
    t(blank(unlabeled, 0), Rest, blank(unlabeled, 1)),
    t(iri('http://example.org/stuff/1.0/a'), iri('http://example.org/stuff/1.0/b'), blank(unlabeled, 0))
  ],
  S20 = ps_b_b([""-"http://example.org/stuff/1.0/"], "", 2),
  S21 = ps_b_b([
    "rdf"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    ""-"http://example.org/stuff/1.0/"
  ], "", 2),
  tag_iri(first, First),
  tag_iri(rest, Rest),
  tag_iri(nil, Nil),
  tag_iri(string, StringTy),
  meta_test_parser_output(In20, Ts20, S20),
  meta_test_parser_output(In21, Ts21, S21),
true.

test_spec_example22 :-
  In = "@prefix : <http://example.org/stuff/1.0/> .\n:a :b \"The first line\\nThe second line\\n  more\" .\n:a :b \"\"\"The first line\nThe second line\n  more\"\"\" .",
  Ts = [
    t(iri('http://example.org/stuff/1.0/a'), iri('http://example.org/stuff/1.0/b'), literal(StringTy, "The first line\nThe second line\n  more")),
    t(iri('http://example.org/stuff/1.0/a'), iri('http://example.org/stuff/1.0/b'), literal(StringTy, "The first line\nThe second line\n  more"))
  ],
  S = ps_b_b([""-"http://example.org/stuff/1.0/"], "", 0),
  tag_iri(string, StringTy),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example23_24 :-
  In23 = "@prefix : <http://example.org/stuff/1.0/> .\n(1 2.0 3E1) :p \"w\" .",
  In24 = "@prefix : <http://example.org/stuff/1.0/> .\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n    _:b0  rdf:first  1 ;\n          rdf:rest   _:b1 .\n    _:b1  rdf:first  2.0 ;\n          rdf:rest   _:b2 .\n    _:b2  rdf:first  3E1 ;\n          rdf:rest   rdf:nil .\n    _:b0  :p         \"w\" . ",
  Ts23 = [
    t(blank(unlabeled, 0), First, literal(IntegerTy, "1")),
    t(blank(unlabeled, 0), Rest, blank(unlabeled, 1)),
    t(blank(unlabeled, 1), First, literal(DecimalTy, "2.0")),
    t(blank(unlabeled, 1), Rest, blank(unlabeled, 2)),
    t(blank(unlabeled, 2), First, literal(DoubleTy, "3E1")),
    t(blank(unlabeled, 2), Rest, Nil),
    t(blank(unlabeled, 0), iri('http://example.org/stuff/1.0/p'), literal(StringTy, "w"))
  ],
  Ts24 = [
    t(blank(labeled, '_:b0'), First, literal(IntegerTy, "1")),
    t(blank(labeled, '_:b0'), Rest, blank(labeled, '_:b1')),
    t(blank(labeled, '_:b1'), First, literal(DecimalTy, "2.0")),
    t(blank(labeled, '_:b1'), Rest, blank(labeled, '_:b2')),
    t(blank(labeled, '_:b2'), First, literal(DoubleTy, "3E1")),
    t(blank(labeled, '_:b2'), Rest, Nil),
    t(blank(labeled, '_:b0'), iri('http://example.org/stuff/1.0/p'), literal(StringTy, "w"))
  ],
  S23 = ps_b_b([""-"http://example.org/stuff/1.0/"], "", 3),
  S24 = ps_b_b([
    "rdf"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    ""-"http://example.org/stuff/1.0/"
  ], "", 0),
  tag_iri(first, First),
  tag_iri(rest, Rest),
  tag_iri(nil, Nil),
  tag_iri(string, StringTy),
  tag_iri(integer, IntegerTy),
  tag_iri(decimal, DecimalTy),
  tag_iri(double, DoubleTy),
  meta_test_parser_output(In23, Ts23, S23),
  meta_test_parser_output(In24, Ts24, S24),
true.

test_spec_example25_26 :-
  In25 = "PREFIX : <http://example.org/stuff/1.0/>\n(1 [:p :q] ( 2 ) ) :p2 :q2 .",
  In26 = "PREFIX : <http://example.org/stuff/1.0/>\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n    _:b0  rdf:first  1 ;\n          rdf:rest   _:b1 .\n    _:b1  rdf:first  _:b2 .\n    _:b2  :p         :q .\n    _:b1  rdf:rest   _:b3 .\n    _:b3  rdf:first  _:b4 .\n    _:b4  rdf:first  2 ;\n          rdf:rest   rdf:nil .\n    _:b3  rdf:rest   rdf:nil .",
  Ts25 = [
    t(blank(unlabeled, 0), First, literal(IntegerTy, "1")),
    t(blank(unlabeled, 0), Rest, blank(unlabeled, 1)),
    t(blank(unlabeled, 2), iri('http://example.org/stuff/1.0/p'), iri('http://example.org/stuff/1.0/q')),
    t(blank(unlabeled, 1), First, blank(unlabeled, 2)),
    t(blank(unlabeled, 1), Rest, blank(unlabeled, 3)),
    t(blank(unlabeled, 4), First, literal(IntegerTy, "2")),
    t(blank(unlabeled, 4), Rest, Nil),
    t(blank(unlabeled, 3), First, blank(unlabeled, 4)),
    t(blank(unlabeled, 3), Rest, Nil),
    t(blank(unlabeled, 0), iri('http://example.org/stuff/1.0/p2'), iri('http://example.org/stuff/1.0/q2'))
  ],
  Ts26 = [
    t(blank(labeled, '_:b0'), First, literal(IntegerTy, "1")),
    t(blank(labeled, '_:b0'), Rest, blank(labeled, '_:b1')),
    t(blank(labeled, '_:b1'), First, blank(labeled, '_:b2')),
    t(blank(labeled, '_:b2'), iri('http://example.org/stuff/1.0/p'), iri('http://example.org/stuff/1.0/q')),
    t(blank(labeled, '_:b1'), Rest, blank(labeled, '_:b3')),
    t(blank(labeled, '_:b3'), First, blank(labeled, '_:b4')),
    t(blank(labeled, '_:b4'), First, literal(IntegerTy, "2")),
    t(blank(labeled, '_:b4'), Rest, Nil),
    t(blank(labeled, '_:b3'), Rest, Nil)
  ],
  S25 = ps_b_b([""-"http://example.org/stuff/1.0/"], "", 5),
  S26 = ps_b_b([
    "rdf"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    ""-"http://example.org/stuff/1.0/"
  ], "", 0),
  tag_iri(first, First),
  tag_iri(rest, Rest),
  tag_iri(nil, Nil),
  tag_iri(integer, IntegerTy),
  meta_test_parser_output(In25, Ts25, S25),
  meta_test_parser_output(In26, Ts26, S26),
true.

test_spec_example27 :-
  In = "\n@prefix ericFoaf: <http://www.w3.org/People/Eric/ericP-foaf.rdf#> .@prefix : <http://xmlns.com/foaf/0.1/> .ericFoaf:ericP :givenName \"Eric\" ;              :knows <http://norman.walsh.name/knows/who/dan-brickley> ,                      [ :mbox <mailto:timbl@w3.org> ] ,                      <http://getopenid.com/amyvdh> .",
  Ts = [
    t(iri('http://www.w3.org/People/Eric/ericP-foaf.rdf#ericP'), iri('http://xmlns.com/foaf/0.1/givenName'), literal(StringTy, "Eric")),
    t(iri('http://www.w3.org/People/Eric/ericP-foaf.rdf#ericP'), iri('http://xmlns.com/foaf/0.1/knows'), iri('http://norman.walsh.name/knows/who/dan-brickley')),
    t(blank(unlabeled, 0), iri('http://xmlns.com/foaf/0.1/mbox'), iri('mailto:timbl@w3.org')),
    t(iri('http://www.w3.org/People/Eric/ericP-foaf.rdf#ericP'), iri('http://xmlns.com/foaf/0.1/knows'), blank(unlabeled, 0)),
    t(iri('http://www.w3.org/People/Eric/ericP-foaf.rdf#ericP'), iri('http://xmlns.com/foaf/0.1/knows'), iri('http://getopenid.com/amyvdh'))
  ],
  S = ps_b_b([
    ""-"http://xmlns.com/foaf/0.1/",
    "ericFoaf"-"http://www.w3.org/People/Eric/ericP-foaf.rdf#"
  ], "", 1),
  tag_iri(string, StringTy),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example28 :-
  In = "\n@prefix dc: <http://purl.org/dc/terms/> .\n@prefix frbr: <http://purl.org/vocab/frbr/core#> .\n<http://books.example.com/works/45U8QJGZSQKDH8N> a frbr:Work ;\n     dc:creator \"Wil Wheaton\"@en ;\n     dc:title \"Just a Geek\"@en ;\n     frbr:realization <http://books.example.com/products/9780596007683.BOOK>,\n         <http://books.example.com/products/9780596802189.EBOOK> .\n<http://books.example.com/products/9780596007683.BOOK> a frbr:Expression ;\n     dc:type <http://books.example.com/product-types/BOOK> .\n<http://books.example.com/products/9780596802189.EBOOK> a frbr:Expression ;\n     dc:type <http://books.example.com/product-types/EBOOK> .",
  Ts = [
    t(iri('http://books.example.com/works/45U8QJGZSQKDH8N'), iri(Type), iri('http://purl.org/vocab/frbr/core#Work')),
    t(iri('http://books.example.com/works/45U8QJGZSQKDH8N'), iri('http://purl.org/dc/terms/creator'), literal(LangStrTy, @("Wil Wheaton", "en"))),
    t(iri('http://books.example.com/works/45U8QJGZSQKDH8N'), iri('http://purl.org/dc/terms/title'), literal(LangStrTy, @("Just a Geek", "en"))),
    t(iri('http://books.example.com/works/45U8QJGZSQKDH8N'), iri('http://purl.org/vocab/frbr/core#realization'), iri('http://books.example.com/products/9780596007683.BOOK')),
    t(iri('http://books.example.com/works/45U8QJGZSQKDH8N'), iri('http://purl.org/vocab/frbr/core#realization'), iri('http://books.example.com/products/9780596802189.EBOOK')),
    t(iri('http://books.example.com/products/9780596007683.BOOK'), iri(Type), iri('http://purl.org/vocab/frbr/core#Expression')),
    t(iri('http://books.example.com/products/9780596007683.BOOK'), iri('http://purl.org/dc/terms/type'), iri('http://books.example.com/product-types/BOOK')),
    t(iri('http://books.example.com/products/9780596802189.EBOOK'), iri(Type), iri('http://purl.org/vocab/frbr/core#Expression')),
    t(iri('http://books.example.com/products/9780596802189.EBOOK'), iri('http://purl.org/dc/terms/type'), iri('http://books.example.com/product-types/EBOOK'))
  ],
  S = ps_b_b([
    "frbr"-"http://purl.org/vocab/frbr/core#",
    "dc"-"http://purl.org/dc/terms/"
  ], "", 0),
  tag_type(a, Type),
  tag_iri(lang_string, LangStrTy),
  meta_test_parser_output(In, Ts, S),
true.

test_spec_example29 :-
  In = "@prefix frbr: <http://purl.org/vocab/frbr/core#> .\n<http://books.example.com/works/45U8QJGZSQKDH8N> a frbr:Work .",
  Ts = [
    t(iri('http://books.example.com/works/45U8QJGZSQKDH8N'), iri(Type), iri('http://purl.org/vocab/frbr/core#Work'))
  ],
  S = ps_b_b(["frbr"-"http://purl.org/vocab/frbr/core#"], "", 0),
  tag_type(a, Type),
  meta_test_parser_output(In, Ts, S),
true.

%%%%%%%%%%%%%%%  END  Parser Spec Examples %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%  END  Parser %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  END  TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_tests :- run_tests("test_").

run_tests(Prefix) :- run_tests(Prefix, test_turtle).

writen(X) :- write(X), nl.
