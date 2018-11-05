/*
Comparison of the Dirichlet distribution with a truncated Dirichlet process.
*/
/** <examples>
?- truncated(2000,1,3,G).
?- dir(2000,1,3,G).
?- comparison(2000,1,3,G).


*/
 :- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

dir_index(K,Alpha,I):-
  findall(Alpha,between(1,K,_),AlphaVec),
  theta(AlphaVec,Theta),
  index(Theta,K,I).

theta(AlphaVec,Theta):dirichlet(Theta,AlphaVec).

index(Theta,K,I):discrete(I,Dist):-
  numlist(1,K,Indexes),
  maplist(pair,Indexes,Theta,Dist).

pair(V,P,V:P).

% dp_stick_index(NV,Alpha,I)
% returns in I the index of the NVth sample from the DP
dp_stick_index(NV,Alpha,I):-
  dp_stick_index(1,NV,Alpha,I).

dp_stick_index(N,NV,Alpha,V):-
  stick_proportion(N,Alpha,P),
  choose_prop(N,NV,Alpha,P,V).
  
% choose_prop(N,NV,Alpha,P,V)
% returns in V the index of the end of the stick breaking process starting
% from index N for the NVth value to be sampled from the DP
choose_prop(N,NV,_Alpha,P,N):-
  pick_portion(N,NV,P).

choose_prop(N,NV,Alpha,P,V):-
  neg_pick_portion(N,NV,P),
  N1 is N+1,
  dp_stick_index(N1,NV,Alpha,V).
 
% sample of the beta_i parameters
stick_proportion(_,Alpha,P):beta(P,1,Alpha).

% flip of the coing for the portion of the stick of size P
pick_portion(N,NV,P):P;neg_pick_portion(N,NV,P):1-P.

:- end_lpad.

truncated(Samples,Alpha,K,Chart):-
  mc_rejection_sample_arg(dp_stick_index(1,Alpha,V),(dp_stick_index(1,Alpha,VV),VV=<K),Samples,V,L),
  histogram(L,Chart).

dir(Samples,Alpha,K,Chart):-
  mc_sample_arg(dir_index(K,Alpha,V),Samples,V,L),
  histogram(L,Chart).

comparison(Samples,Alpha,K,Chart):-
   mc_rejection_sample_arg(dp_stick_index(1,Alpha,V),(dp_stick_index(1,Alpha,VV),VV=<K),Samples,V,LP),
  mc_sample_arg(dir_index(K,Alpha,V),Samples,V,LD),
  densities(LD,LP,Chart).


