:- use_module(library(mcintyre)).
:- use_module(library(matrix)).
:- use_module(library(clpfd)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

% http://arxiv.org/pdf/1505.02965v2.pdf


gp_predict(XP,Kernel,XT,YT,YP):-
  compute_cov(XT,Kernel,C),
  matrix_inversion(C,C_1),
  transpose([YT],YST),
  matrix_multiply(C_1,YST,C_1T),
  gp_predict_single(XP,Kernel,XT,C_1T,YP).

gp_predict_single([],_,_,_,[]).

gp_predict_single([XH|XT],Kernel,X,C_1T,[YH|YT]):-
  compute_k(X,XH,Kernel,K),
  matrix_multiply([K],C_1T,[[YH]]),
  gp_predict_single(XT,Kernel,X,C_1T,YT).

compute_k([],_,_,[]).

compute_k([XH|XT],X,Ker,[HK|TK]):-
  call(Ker,XH,X,HK),
  compute_k(XT,X,Ker,TK).
  

gp(X,Kernel,Y):-
  compute_cov(X,Kernel,C),
  gp(C,Y).

gp(Cov,Y):gaussian(Y,Mean,Cov):-
  length(Cov,N),
  list0(N,Mean).

compute_cov(X,Kernel,C):-
  length(X,N),
  cov(X,N,Kernel,CT,CND),
  transpose(CND,CNDT),
  matrix_sum(CT,CNDT,C).

cov([],_,_,[],[]).

cov([XH|XT],N,Ker,[KH|KY],[KHND|KYND]):-
  length(XT,LX),
  N1 is N-LX-1,
  list0(N1,KH0),
  cov_row(XT,XH,Ker,KH1),
  call(Ker,XH,XH,KXH),
%  kernel(XH,XH,KXH),
  append([KH0,[KXH],KH1],KH),
  append([KH0,[0],KH1],KHND),
  cov(XT,N,Ker,KY,KYND).

cov_row([],_,_,[]).

cov_row([H|T],XH,Ker,[KH|KT]):-
  call(Ker,H,XH,KH),
  cov_row(T,XH,Ker,KT).

sq_exp_p(X,XP,K):-
  sigma(Sigma),
  l(L),
  K is Sigma^2*exp(-((X-XP)^2)/2/(L^2)).


l(L):uniform(L,[1,2,3]).

sigma(Sigma):uniform(Sigma,-2,2).

:- end_lpad.

%kernel(X,X,K):-!,
%  K is 1.27^2+0.3^2.

sq_exp(X,XP,K):-
  K is exp(-((X-XP)^2)/2).
  %K is (1.27^2)*exp(-((X-XP)^2)/2).

min(X,XP,K):-
  K is min(X,XP).

lin(X,XP,K):-
  K is (X*XP+5)^2.

ou(X,XP,K):-
  K is exp(-abs(X-XP)).
draw_fun(Kernel,C):-
%  X=[-1.50,-1.00,-0.75,-0.40,-0.25,0.00],
%  X=[-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1.00,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4,4.5],
  X=[-3,-2,-1,0,1,2,3],
  compute_cov(X,Kernel,K),
  mc_sample_arg_first(gp(K,Y),5,Y,L),
  numlist(1,5,LD),
  maplist(name_s,L,LD,L1),
  C = c3{data:_{x:x, columns:[[x|X]|L1] },
 % legend:_{show: false},
  axis:_{ x:_{ tick:_{fit:false}}}}.

draw_fun_post(Kernel,C):-
%  X=[-1.50,-1.00,-0.75,-0.40,-0.25,0.00],
%  X=[-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1.00,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4,4.5],
  numlist(0,10,X),
  XT=[2,4,6,8,9],
  YT=[1,-0.4,-0.8,0.25,0.6],
  mc_lw_sample_arg(gp(X,Kernel,Y),gp(XT,Kernel,YT),100,Y,L),
  keysort(L,LS),
  reverse(LS,[Y1-_,Y2-_,Y3-_,Y4-_,Y5-_|_]),
  C = c3{data:_{xs:_{y:xt,f1:x,f2:x,f3:x,f4:x,f5:x}, 
  columns:[[y|YT],[xt|XT],[x|X],[f1|Y1],[f2|Y2],[f3|Y3],[f4|Y4],
    [f5|Y5]],
    types:_{f1: spline,f2:spline,f3:spline,f4:spline,
    f5:spline,y:scatter}},
 % legend:_{show: false},
  axis:_{ x:_{ tick:_{fit:false}}}}.

draw_fun_postp(Kernel,C):-
%  X=[-1.50,-1.00,-0.75,-0.40,-0.25,0.00],
%  X=[-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1.00,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4,4.5],
  numlist(0,10,X),
  XT=[2.5,6.5,8.5],
  YT=[1,-0.8,0.6],
  mc_lw_sample_arg(gp_predict(X,Kernel,XT,YT,Y),gp(XT,Kernel,YT),5,Y,L),
  keysort(L,LS),
  reverse(LS,[Y1-_,Y2-_,Y3-_|_]),
  C = c3{data:_{xs:_{y:xt,f1:x,f2:x,f3:x}, 
  columns:[[y|YT],[xt|XT],[x|X],[f1|Y1],[f2|Y2],[f3|Y3]],
    types:_{f1: spline,f2:spline,f3:spline,y:scatter}},
 % legend:_{show: false},
  axis:_{ x:_{ tick:_{fit:false}}}}.



name_s(V-_,N,[ND|V]):-
  atomic_concat(f,N,ND).

/** <examples>
?- draw_fun_post(sq_exp_p,C).
?- draw_fun_postp(sq_exp_p,C).
*/
