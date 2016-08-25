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

kf_fin(N,O, T) :-
  init(S),
  kf_part(0, N, S,O,_LS,T).


kf(N,O,LS) :-
  init(S),
  kf_part(0, N, S,O,LS,_T).

kf_part(I, N, S, [V|RO], [S|LS], T) :-
  I < N, 
  NextI is I+1,
  trans(S,I,NextS),
  emit(NextS,I,V),
  kf_part(NextI, N, NextS, RO, LS, T).

kf_part(N, N, S, [], [], S).

trans(S,I,NextS) :-
  {NextS =:= E + S},
  trans_err(I,E).

emit(NextS,I,V) :-
  {V =:= NextS +X},
  obs_err(I,X).

init(S):gaussian(S,0,1).
% prior as in Russel and Norvig 2010, Fig 15.10
trans_err(_,E):gaussian(E,0,2).
% transition noise as in Russel and Norvig 2010, Fig 15.10
obs_err(_,E):gaussian(E,0,1).
% observation noise as in Russel and Norvig 2010, Fig 15.10

:- end_lpad.

hist(Samples,NBins,Chart):-
  mc_sample_arg(kf_fin(1,_O1,Y),Samples,Y,L0),
  histogram(L0,NBins,Chart).

dens_lw(Samples,NBins,Chart):-
  mc_sample_arg(kf_fin(1,_O1,Y),Samples,Y,L0),
  mc_lw_sample_arg(kf_fin(1,_O2,T),kf_fin(1,[2.5],_T),Samples,T,L),
  densities(L0,L,NBins,Chart).
% plot the density of the state at time 1 in case of no observation (prior)
% and in case of observing 2.5.
% Observation as in Russel and Norvig 2010, Fig 15.10

filter(Samples,NBins,C):-
  sample_trajectory(4,O,St),
  mc_lw_sample_arg(kf(4,_O,T),kf_fin(4,O,_T),Samples,T,L),
  maplist(separate,L,T1,T2,T3,T4),
  density(T1,NBins,C1),
  density(T2,NBins,C2),
  density(T3,NBins,C3),
  density(T4,NBins,C4),
  [[x|X1],[dens|S1]]=C1.data.columns,
  [[x|X2],[dens|S2]]=C2.data.columns,
  [[x|X3],[dens|S3]]=C3.data.columns,
  [[x|X4],[dens|S4]]=C4.data.columns,
  append([S1,S2,S3,S4],AllD),
  max_list(AllD,Max),
  maplist(is,Y,[Max+50,Max+100,Max+150,Max+200]),
  C = c3{data:_{xs:_{t:xt,o:xo,s1:x1,s2:x2,s3:x3,s4:x4},
  columns:[[xt|St],[t|Y],
    [xo|O],[o|Y],
    [x1|X1],[s1|S1],
    [x2|X2],[s2|S2],
    [x3|X3],[s3|S3],
    [x4|X4],[s4|S4]],
    types:_{s1: spline,s2: spline,s3: spline,s4: spline,t:scatter,o:scatter}},
 % legend:_{show: false},
  axis:_{ x:_{ tick:_{fit:false}}}}.

separate([S1,S2,S3,S4]-W,S1-W,S2-W,S3-W,S4-W).

sample_trajectory(N,Ob,St):-
  mc_sample_arg(kf(N,O,T),1,(O,T),L),
  L=[[(Ob,St)]-_].
/** <examples>
?- filter(1000,40,C).
?- dens_lw(1000,40,G).
% plot the density of the state at time 1 in case of no observation (prior)
% and in case of observing 2.5 by taking 1000 samples and dividing the domain
% in 40 bins
?- hist(1000,40,G).
% plot the density of the state at time 1 in case of no observation
% by taking 1000 samples and dividing the domain
% in 40 bins


*/
 
