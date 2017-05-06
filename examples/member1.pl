:- use_module(sldnf_draw).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(sldnf).
:- endif.

:- sldnf.

:- begin_program.

member1(X ,[X|_T]).
member1(X ,[_H|T]):-
member1(X,T).

:-end_program.

:-begin_query.

member1(X,[1,2]), \+ member1(X,[1,3]).

:-end_query.

/** <examples>

?- draw_goal(Tree).


*/
