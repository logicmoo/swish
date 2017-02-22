:- if(current_predicate(use_rendering/1)).
:- use_rendering(tiles).
:- endif.

main(A):-
    A=[[grass,water,grass],[water,grass,grass],[water,water,grass]].
