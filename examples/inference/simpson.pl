:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(graphviz).
:- endif.

:- pita.

:- begin_lpad.
:- action drug/0,nodrug/0,female/0.
female:0.5.

recovery:0.6:- drug,male.
recovery:0.7:- nodrug,male.

recovery:0.2:- drug,female.
recovery:0.3:- nodrug,female.

drug:30/40:- male.
drug:10/40:-female.

nodrug:- \+ drug.
male:- \+ female.

:-end_lpad.

/** <examples>



?- prob(recovery,drug,P).
%P = 0.49999999999999994.

?- prob(recovery,\+ drug,P).
%P = 0.39999999999999997.

?- prob(recovery,(drug,female),P).
%P = 0.2.

?- prob(recovery,(\+drug,female),P).
%P = 0.3.

?- prob(recovery,(drug,\+female),P).
%P = 0.6.

?- prob(recovery,(\+ drug,\+female),P).
%P = 0.7.

?- prob(recovery,do(drug),P).
%P = 0.4.

?- prob(recovery,do(\+ drug),P).
%P = 0.5.

?- prob(recovery,(do(drug),female),P).
%P = 0.2.

?- prob(recovery,(do(\+drug),female),P).
%P = 0.3.

?- prob(recovery,(do(drug),\+ female),P).
%P = 0.6.

?- prob(recovery,(do(\+ drug),\+ female),P).
%P = 0.7.

*/
