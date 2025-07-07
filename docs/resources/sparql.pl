% NOTE: if the prolog implementation supports "soft cut"
% optional could be implemented with it
%
% For example SWI-Prolog supports "soft cut" (*->)/2
% (docs) https://www.swi-prolog.org/pldoc/doc_for?object=(*-%3E)/2
%
optional(Sub, Pred, Obj) :- ( rdf(Sub, Pred, Obj) *-> true ; false ).
%
% For example SICStus-Prolog supports "soft cut" if/3
% (docs) https://sicstus.sics.se/sicstus/docs/4.1.2/html/sicstus/ref_002dsem_002dctr_002dite.html
optional(Sub, Pred, Obj) :- if(rdf(Sub, Pred, Obj), true, false).
%
% otherwise
%
optional(Sub, Pred, Obj) :- ( \+ rdf(Sub, Pred, Obj) -> true ; rdf(Sub, Pred, Obj) ).

filter_not_exists(Sub, Pred, Obj) :- \+ rdf(Sub, Pred, Obj).

filter_exists(Sub, Pred, Obj) :- \+ \+ rdf(Sub, Pred, Obj).
