/*
Latent Dirichlet Allocation

See https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation
*/

/** <example>
?- prob_topic_1(G).
?- mc_sample_arg_bar(word(1,1,W),100,W,G).
?- mc_sample_arg_bar((word(1,1,W),topic(1,1,T)),100,(W,T),G).
?- mc_sample_arg_bar(topic(1,1,T),100,T,G).
?- mc_mh_sample_arg_bar(topic(1,1,T),(word(1,1,1),word(1,2,1)),100,2,T,G).

*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- mc.

:- begin_lpad.

theta(_,Theta):dirichlet(Theta,Alpha):-
  alpha(Alpha).

topic(DocumentID,_,Topic):discrete(Topic,Dist):-
  theta(DocumentID,Theta),
  topic_list(Topics),
  maplist(pair,Topics,Theta,Dist).

word(DocumentID,WordID,Word):discrete(Word,Dist):-
  topic(DocumentID,WordID,Topic),
  beta(Topic,Beta),
  word_list(Words),
  maplist(pair,Words,Beta,Dist).

beta(_,Beta):dirichlet(Beta,Parameters):-
  n_words(N),
  eta(Eta),
  findall(Eta,between(1,N,_),Parameters).

alpha(Alpha):-
  eta(Eta),
  n_topics(N),
  findall(Eta,between(1,N,_),Alpha).

eta(2).

pair(V,P,V:P).

topic_list(L):-
  n_topics(N),
  numlist(1,N,L).

word_list(L):-
  n_words(N),
  numlist(1,N,L).


n_topics(2).

n_words(10).

:-end_lpad.

prob_topic_1(G):-
  mc_sample_arg(theta(1,[T0|_]),400,T0,L0),
  mc_mh_sample_arg(theta(1,[T1|_]),(word(1,1,1),word(1,2,1)),400,2,T1,L1),
  densities(L0,L1,30,G).
