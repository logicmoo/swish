:- use_module(library(mcintyre)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(tiles).
:- endif.

/** <examples>

?- mc_sample_arg_first(map(3,4,M),1,M,[ Map- _ ]).

*/


:-mc.

:-begin_lpad.
main(A):-
    A=a(a(grass,water,grass),a(water,grass,grass),a(water,water,grass)).

map(H,W,M):-
  length(Rows,H),
  M=..[map|Rows],
  foldl(select(W),Rows,1,_).

select(W,Row,N0,N):-
  length(RowL,W),
  N is N0+1,
  Row=..[row|RowL],
  foldl(pick_row(N),RowL,1,_).

pick_row(N,T,M0,M):-
  M is M0+1,
  pick_tile(N,M,T).

pick_tile(_,_,T):uniform(T,Tiles):-tiles(Tiles).

tiles(T):-
  findall(Tile, tile(Tile,_),T).

tile(grass,[]).
tile(water,[]).

:-end_lpad.
