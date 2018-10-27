/* computation of the h-index of an author given his number of papers
assuming that the citations per paper follow a Poisson distribution with 
given average.
*/
:- use_module(library(mcintyre)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.

:- begin_lpad.

citations(AverageCit,_,Cit):poisson(Cit,AverageCit).

h_index(NumPapers,AverageCit,HIndex):-
    numlist(1,NumPapers,Papers),
    maplist(citations(AverageCit),Papers,Citations),
    sort(0,  @>=, Citations,  Sorted),
    compute_index(Sorted,1,HIndex).

compute_index([0|_],_,0):-!.

compute_index([],I0,I):-!,
    I is I0-1.

compute_index([I|_T],I,I):-!.

compute_index([H|_T],I0,I):-
    H<I0,!,
    I is I0-1.

compute_index([H|T],I0,I):-
    H>I0,
    I1 is I0+1,
    compute_index(T,I1,I).


    
:- end_lpad.

/** <examples> Your example queries go here, e.g.

?- mc_sample_arg_first(h_index(200,10,H),1000,H,HList),argbar(HList,Bar).
compute the distribution of the h_index given that the authors wrote
200 papers and each paper receives on average 10 citations
?- mc_expectation(h_index(200,10,H),1000,H,HExp).
compute the expected value of the h_index given that the authors wrote
200 papers and each paper receives on average 10 citations
*/

