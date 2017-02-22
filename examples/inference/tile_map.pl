:- use_module(library(mcintyre)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(tiles).
:- endif.

/** <examples>

?- mc_mh_sample_arg(map(3,4,M),constraints(3,4),1,10,2,M,[[Map] - _]).
?- mc_sample_arg_first(map(3,4,M),1,M,[ Map- _ ]).

*/


:-mc.

:-begin_lpad.

map(H,W,M):-
  tiles(Tiles),
  length(Rows,H),
  M=..[map,Tiles|Rows],
  foldl(select(H,W),Rows,1,_).

select(H,W,Row,N0,N):-
  length(RowL,W),
  N is N0+1,
  Row=..[row|RowL],
  foldl(pick_row(H,W,N0),RowL,1,_).

pick_row(H,W,N,T,M0,M):-
  M is M0+1,
  pick_tile(N,M0,H,W,T).

% constraints on map generation
% the center tile is water
pick_tile(HC,WC,H,W,water):-
  HC is H//2,
  WC is W//2,!.

% on the left border grass is more likely than water
pick_tile(1,X,H,W,grass):0.7;pick_tile(1,X,H,W,water):0.3.

% on the bottom border water is more likely than water
pick_tile(Y,X,H,W,grass):0.3;pick_tile(Y,X,H,W,water):0.7:-
  Y=H.

% on the other places tiles are chosen uniformly at random
pick_tile(Y,_,H,_,T):uniform(T,Tiles):-
  Y=\=1,
  Y=\=H,
  tile_names(Tiles).

% constraints after map generation (soft constraints)
% tiles adjacent to water are more probably water
% pick_tile(Y,X,H,W,water),adjacent(Y,X,Y1,X1,H,W)=>pick_tile(Y1,X1,H,W,water)

constraint_water(Y,X,_Y1,_X1,H,W):-
  pick_tile(Y,X,H,W,T),
  T \= water.

constraint_water(_Y,_X,Y1,X1,H,W):-
  pick_tile(Y1,X1,H,W,water).

constraint_water(Y,X,Y1,X1,H,W):0.1:-
  pick_tile(Y,X,H,W,water),
  adjacent(Y,X,Y1,X1,H,W),
  \+ pick_tile(Y1,X1,H,W,water).

constraints(H,W):-
  HC is H//2,
  WC is W//2,
  H1 is HC-1,
  H2 is HC+1,
  W1 is WC-1,
  W2 is WC+1,
  findall((Y,X,Y1,X1),(
    between(H1,H2,Y),between(W1,W2,X),adjacent(Y,X,Y1,X1,H,W)),L),
  maplist(call_const(H,W),L).

call_const(H,W,(Y,X,Y1,X1)):-
  constraint_water(Y,X,Y1,X1,H,W).

:-end_lpad.

adjacent(Y,X,Y1,X1,H,W):-
  left(Y,X,Y1,X1,H,W).

adjacent(Y,X,Y1,X1,H,W):-
  right(Y,X,Y1,X1,H,W).

adjacent(Y,X,Y1,X1,H,W):-
  above(Y,X,Y1,X1,H,W).

adjacent(Y,X,Y1,X1,H,W):-
  below(Y,X,Y1,X1,H,W).

left(Y,X,Y,X1,_,_):-
  X>1,
  X1 is X-1.

right(Y,X,Y,X1,_,W):-
  X<W,
  X1 is X+1.

above(Y,X,Y1,X,_,_):-
  Y>1,
  Y1 is Y-1.

below(Y,X,Y1,X,H,_):-
  Y<H,
  Y1 is Y+1.

tile_names(T):-
  findall(Tile,tile(Tile,_URL),T).

tiles(T):-
  findall(tile(Tile,URL),tile(Tile,URL),T).

tile(grass,'/icons/tiles/grass.png').
tile(water,'/icons/tiles/water.png').
tile(rock,'/icons/tiles/rock.png').
tile(tree,'/icons/tiles/tree.png').
