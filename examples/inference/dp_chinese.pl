/*
One-dimensional  Kalman filter. Hidden Markov model with a real
value as state and a real value as output. The next state is given by
the current state plus Gaussian noise (mean 0 and variance 2 in this example)
and the output is given by the current state plus Gaussian noise (mean
0 and variance 1 in this example). 
This example can be considered as modeling a random walk of a single continuous 
state variable with noisy observations. 
Given that at time 0 the value 2.5 was
observed, what is the distribution of the state at time 1 (filtering problem)?
The distribution of the state is plotted in the case of having (posterior) or 
not having the observation (prior).
Liklihood weighing is used to condition the distribution on evidence on
a continuous random variable (evidence with probability 0).
CLP(R) constraints allow both sampling and weighing samples with the same
program.
From
Islam, Muhammad Asiful, C. R. Ramakrishnan, and I. V. Ramakrishnan. 
"Inference in probabilistic logic programs with continuous random variables." 
Theory and Practice of Logic Programming 12.4-5 (2012): 505-523.
http://arxiv.org/pdf/1112.2681v3.pdf
Russell, S. and Norvig, P. 2010. Arficial Intelligence: A Modern Approach. 
Third Edition, Prentice Hall, Figure 15.10 page 587

*/
:- use_module(library(mcintyre)).
:- use_module(library(clpr)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

dp_n_values(N,N,_Alpha,[],Counts,Counts):-!.

dp_n_values(N0,N,Alpha,[[V]-1|Vs],Counts0,Counts):-
  N0<N,
  dp_value(N0,Alpha,Counts0,V,Counts1),
  N1 is N0+1,
  dp_n_values(N1,N,Alpha,Vs,Counts1,Counts).
  
dp_value(NV,Alpha,Counts,V,Counts1):-
  draw_sample(Counts,NV,Alpha,I),
  update_counts(0,I,Alpha,Counts,Counts1),
  dp_pick_value(I,NV,V).


update_counts(_I0,_I,Alpha,[_C],[1,Alpha]):-!.


update_counts(I,I,_Alpha,[C|Rest],[C1|Rest]):-
  C1 is C+1.

update_counts(I0,I,Alpha,[C|Rest],[C|Rest1]):-
  I1 is I0+1,
  update_counts(I1,I,Alpha,Rest,Rest1).

draw_sample(Counts,NV,Alpha,I):-
  NS is NV+Alpha,
  maplist(div(NS),Counts,Probs),
  length(Counts,LC),
  numlist(1,LC,Values),
  maplist(pair,Values,Probs,Discrete),
  take_sample(NV,Discrete,I).

take_sample(_,D,V):discrete(V,D).

dp_pick_value(I,NV,V):-
  ivar(I,IV),
  Var is 1.0/IV,
  mean(I,Var,M),
  value(NV,M,Var,V).

ivar(_,IV):gamma(IV,1,0.1).

mean(_,V0,M):gaussian(M,0,V):-V is V0*30.

value(_,M,V,Val):gaussian(Val,M,V).


:- end_lpad.
obs0([
-1,7,3]).


obs1([
-1,-1,-1,
7,7,7,7,7,
3,3,3,3,3]).

obs([
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3]).


hist_val(Samples,NBins,Chart):-
  mc_sample_arg_first(dp_n_values(0,Samples,10.0,V),1,V,L),
  L=[Vs-_],
  histogram(Vs,NBins,Chart).

prior(Samples,NBins,Chart):-
  mc_sample_arg_first(dp_n_values(0,100,10.0,T,[10.0],_),Samples,T,L),
  maplist(to_list,L,L1),
  append(L1,Vs),
%  L=[Vs-_],
  histogram(Vs,NBins,-8,15,Chart).
%  
post(Samples,NBins,Chart):-
  obs0(O),
  maplist(to_val,O,O1),
  length(O1,N),
%  mc_sample_arg(dp_value(0,10.0,T),Samples,T,L0),
  mc_lw_sample_arg_log(dp_value(0,10.0,[10.0],T,_),dp_n_values(0,N,10.0,O1,[10.0],_),Samples,T,L),
  maplist(keys,L,LW),
  min_list(LW,Min),
  maplist(exp(Min),L,L1),
%  maplist(to_list,L,L1),
%  append(L1,Vs),
%  L=[Vs-_],
  density(L1,-8,15,NBins,Chart).

exp(Min,L-W,L-W1):- W1 is exp(W-Min).
to_list(L-W,L1):-
  maplist(app_w(W),L,L1).

keys(_-W,W).
div(Den,V,P):- P is V/Den.
pair(A,B,A:B).
app_w(W,V-_,V-W).
to_val(V,[V]-1).
% plot the density of the state at time 1 in case of no observation (prior)
% and in case of observing 2.5.
% Observation as in Russel and Norvig 2010, Fig 15.10

/** <examples>
% plot the density of the state at time 1 in case of no observation (prior)
% and in case of observing 2.5 by taking 1000 samples and dividing the domain
% in 40 bins
?- prior(100,40,G).
?- post(100,40,G).
?- hist(100,40,G).
?- hist_val(100,40,G).
?- hist_repeated(100,40,G).
% plot the density of the state at time 1 in case of no observation
% by taking 1000 samples and dividing the domain
% in 40 bins

?- dens_lw(1000,40,G).

*/
 
