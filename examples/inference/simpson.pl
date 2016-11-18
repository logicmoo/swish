:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.
:- dynamic drug/2.

:- begin_lpad.
female:0.5.

recovery:0.6:- drug,male.
recovery:0.7:- nodrug,male.

recovery:0.2:- drug,female.
recovery:0.3:- nodrug,female.

drug:30/40:- male.
drug:10/40:-female.

nodrug:- \+ drug.
male:- \+ female.

drug_male:- drug, male.
nodrug_male:- nodrug, male.
drug_female:- drug,female.
nodrug_female:- nodrug,female.
:-end_lpad.

/** <examples>
?- prob(recovery,drug,P).
%P = 0.49999999999999994.

?- prob(recovery,nodrug,P).
%P = 0.39999999999999997.

?- prob(recovery,drug_male,P).
%P = 0.6.

?- prob(recovery,nodrug_male,P).
%P = 0.7.

?- prob(recovery,drug_female,P).
%P = 0.2.

?- prob(recovery,nodrug_female,P).
%P = 0.3.

?- prob(recovery,male,P).
%P = 0.6.

?- prob(recovery,female,P).
%P = 0.2.

?- prob(recovery,P).
%P = 0.4.

*/
