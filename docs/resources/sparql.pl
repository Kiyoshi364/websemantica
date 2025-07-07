% NOTE: if the implementation supports soft-cut (*->)/2
% it could be implemented as
%
optional(Sub, Pred, Obj) :- ( rdf(Sub, Pred, Obj) *-> true ; false ).
%
% For example SWI-Prolog supports it
% (documentation) https://www.swi-prolog.org/pldoc/doc_for?object=(*-%3E)/2
%
% otherwise
%
optional(Sub, Pred, Obj) :- ( \+ rdf(Sub, Pred, Obj) -> true ; rdf(Sub, Pred, Obj) ).

filter_not_exists(Sub, Pred, Obj) :- \+ rdf(Sub, Pred, Obj).

filter_exists(Sub, Pred, Obj) :- \+ \+ rdf(Sub, Pred, Obj).
