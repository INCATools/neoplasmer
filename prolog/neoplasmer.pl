/** <module> neoplasmer

  Neoplasm Entity Recognition

*/

:- encoding(utf8).

:- module(neoplasmer,
          [term_best_match/4]).

:- use_module(library(index_util)).
:- use_module(library(rdf_matcher)).
:- use_module(library(tabling)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_sandbox)).

:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(neoplasmer:term_best_match(_,_,_,_)).

:- rdf_register_prefix('MONDO','http://purl.obolibrary.org/obo/MONDO_').
:- rdf_register_prefix('HP','http://purl.obolibrary.org/obo/HP_').
:- rdf_register_prefix('NCIT','http://purl.obolibrary.org/obo/NCIT_').
:- rdf_register_prefix('DOID','http://purl.obolibrary.org/obo/DOID_').
:- rdf_register_prefix('MESH','http://purl.obolibrary.org/obo/MESH_').
:- rdf_register_prefix('GARD','http://purl.obolibrary.org/obo/GARD_').
:- rdf_register_prefix('SCTID','http://purl.obolibrary.org/obo/SCTID_').
:- rdf_register_prefix('UMLS','http://linkedlifedata.com/resource/umls/id/').
:- rdf_register_prefix('MEDGEN','http://purl.obolibrary.org/obo/MEDGEN_').
:- rdf_register_prefix('ONCOTREE','http://purl.obolibrary.org/obo/ONCOTREE_').
:- rdf_register_prefix('EFO','http://www.ebi.ac.uk/efo/EFO_').
:- rdf_register_prefix('Orphanet','http://www.orpha.net/ORDO/Orphanet_').
:- rdf_register_ns(oio,'http://www.geneontology.org/formats/oboInOwl#').

% TODO: move
%! uri_prefix(?URI:atom, ?Prefix:atom)
uri_prefix(URI,Prefix) :-
        rdf_global_id(Prefix:_,URI).

% TODO: move to own lib
call_unique(G) :- setof(G,G,Gs),member(G,Gs).

:- multifile relational_adj_ra/3.


% TODO: move this into rdf_matcher

normalize(V,V2,0) :-    replace_term(V,'ö',o,V2).
normalize(V,V2,0) :-    replace_term(V,'ö',oe,V2).
normalize(V,V2,0) :-    replace_term(V,'-',' ',V2).
normalize(V,V2,0) :-    replace_term(V,'\'s','',V2).
normalize(V,V2,1) :-    replace_term(V,disorder,disease,V2).
normalize(V,V2,1) :-    relational_adj_ra(Adj,Noun,_),atom_concat(Adj,' ',Adj1),atom_concat(Noun,' ',Noun1),replace_term(V,Noun1,Adj1,V2).
normalize(V,V2,0) :-    replace_term(V,tumors,tumor,V2).
normalize(V,V2,0) :-    replace_term(V,tumour,tumor,V2).
normalize(V,V2,1) :-    replace_term(V,tumor,neoplasm,V2).
normalize(V,V2,0) :-    concat_atom([A,B],' of ',V),concat_atom([B,A],' ',V2).
normalize(V,V,0).
normalize(V,V2,5) :- concat_atom([A,B],'/',V), (V2=A;V2=B).
normalize(V,V2,3) :- concat_atom([A,B],' - ',V), (V2=A;V2=B).
normalize(V,V2,5) :- concat_atom([V2,_|_],', ',V).

:- table recursive_normalize/3.
recursive_normalize(V,V2,P) :- normalize(V,V2,P).
recursive_normalize(V,V2,P) :- normalize(V,Z,P1), Z\=V, recursive_normalize(Z,V2,P2), P is P1+P2, P > 0.

replace_term(V,Src,Tgt,V2) :-
        concat_atom(L,Src,V),
        L=[_,_|_],
        concat_atom(L,Tgt,V2).

%! term_best_match(+Term, ?MatchCls, ?MatchScore, ?MatchClsPrefix) is nondet
term_best_match(V, C, S, P) :-
        debug(matcher,'Matching: ~w',[V]),
        setof(C-S,term_match_score(V, C, S, P),Pairs),
        member(C-S,Pairs),
        \+ ((member(_-S2,Pairs),S2>S)).

:- table term_match_score/3.
term_match_score(V, C, S, P) :-
        mutate(_, label, V, V2),
        call_unique(recursive_normalize(V2,V3,Penalty)),
        debug(matcher,'  Normalized: ~w -> ~w',[V,V3]),
        term_match_score1(V3, C, S1),
        uri_prefix(C, P),
        S is S1-Penalty.


term_match_score1(V, C, S) :-
        tr_annot(C,P,V,_,_,_),
        prop_score(P,S).
term_match_score1(V, C, S) :-
        atom_concat(V,' cancer',V2),
        tr_annot(C,P,V2,_,_,_),
        prop_score(P,S).
term_match_score1(V, C, S) :-
        atom_concat(V,' (disease)',V2),
        tr_annot(C,P,V2,_,_,_),
        prop_score(P,S).
term_match_score1(V, C, S) :-
        atom_concat(V,' cancer (disease)',V2),
        tr_annot(C,P,V2,_,_,_),
        prop_score(P,S).
term_match_score1(V, C, S) :-
        atom_concat(V,' neoplasm',V2),
        tr_annot(C,P,V2,_,_,_),
        prop_score(P,S).
term_match_score1(V, C, S) :-
        atom_concat(V,', nos',V2),
        tr_annot(C,P,V2,_,_,_),
        prop_score(P,S).
term_match_score1(V, C, S) :-
        atom_concat('all ',V2, V),
        tr_annot(C,P,V2,_,_,_),
        prop_score(P,S).
term_match_score1(V, C, S) :-
        atom_concat(V,' malignant neoplasm',V2),
        tr_annot(C,P,V2,_,_,_),
        prop_score(P,S).
term_match_score1(V, C, S) :-
        atom_concat('unknown ',V2,V),
        tr_annot(C,P,V2,_,_,_),
        prop_score(P,S).
term_match_score1(V, C, S) :-
        atom_concat('primary ',V2,V),
        tr_annot(C,P,V2,_,_,_),
        prop_score(P,S).
term_match_score1(_,null,-99).

prop_score(label,10).
prop_score(exact,8).
prop_score(related,4).
prop_score(broad,1).
prop_score(narrow,1).


all_matches(V,C,S,P) :-
        t(V),
        term_best_match(V,C,S,P).
all_matches(V,null,-99,null) :-
        t(V),
        \+ term_match_score(V,_,_,_).



/*

  

*/
