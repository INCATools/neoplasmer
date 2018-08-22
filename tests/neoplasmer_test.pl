:- use_module(library(neoplasmer)).
:- use_module(library(rdf_matcher)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).

:- rdf_register_prefix(x,'http://example.org/x/').
:- rdf_register_prefix(y,'http://example.org/y/').
:- rdf_register_prefix(z,'http://example.org/z/').

:- debug(index).

:- begin_tests(rdf_matcher,
               [setup(load_test_file_and_index),
                cleanup(rdf_retractall(_,_,_,_))]).

load_test_file_and_index :-
        rdf_load('tests/data/mondo_subset.owl'),
        index_pairs(tmp).


showall(G) :-
        forall(G,
               format('~q.~n',[G])).

%test(ann) :-
%        showall(tr_annot(_,_,_,_,_,_)).

test(match) :-
        forall(expected(Term,ExpectedMatches),
               check_results(Term,ExpectedMatches)).

check_results(Term,ExpectedMatches) :-
        setof(S-C,P^term_best_match(Term,C,S,P),Pairs),
        !,
        format('Matches(~w) = ~w~n',[Term,Pairs]),
        findall(Cx,(member(_-C,Pairs),uri2id(C,Cx)),Cs),
        assertion(Cs == ExpectedMatches).
check_results(Term,_) :-
        format(user_error,'No matches: ~w',[Term]).


uri2id(Uri,Id) :-
        rdf_global_id(Pre:Local,Uri),
        concat_atom([Pre,:,Local],Id).


expected('neoplasm', ['MONDO:0005070']).
expected('neoplasms', ['MONDO:0005070']).
expected('NEOPLASM', ['MONDO:0005070']).
expected('Pilocytic astrocytoma', ['MONDO:0016691']).
expected('astrocytoma', ['MONDO:0019781']).

:- end_tests(rdf_matcher).
    
