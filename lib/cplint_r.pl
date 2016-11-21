/*  Part of Cplint on SWISH

    Author:        Franco Masotti
    E-mail:        franco.masotti@student.unife.it
    WWW:           <http://github.com/frnmst/swish>
    Copyright (c)  2016
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(cplint_r,
          [ histogram_r/2
          ]).

:- use_module(library(r/r_call)).
:- use_module(library(r/r_data)).
:- use_module(swish(r_swish)).


/*********** 
 * Helpers *
 ***********/


/* R */

load_r_env :-
    <- library("ggplot2").


/* Lists */

to_pair([E]-W,E-W):- !.
to_pair(E-W,E-W).

key(K-_,K).

y(_ - Y,Y).


/***************************************** 
 * Plot predicates ***********************
 *****************************************
 * grep -l histogram *.pl | less *********
 *****************************************/


/**
 * histogram_r(+List:list,+NBins:int) is det
 *
 * Draws a histogram of the samples in List dividing the domain in
 * NBins bins. List must be a list of couples of the form [V]-W or V-W
 * where V is a sampled value and W is its weight.
 */
/* Input controls. Don't change. */
histogram_r(L0,NBins) :-
    load_r_env,
    maplist(to_pair,L0,L1),
    maplist(key,L1,L2),
    l2 <- L2,
    Max <- max(l2),
    Min <- min(l2),
    histogram_r(L0,NBins,Min,Max),
    r_download.

/**
 * histogram_r(+List:list,+NBins:int,+Min:float,+Max:float) is det
 *
 * Draws a histogram of the samples in List dividing the domain in
 * NBins bins. List must be a list of couples of the form [V]-W or V-W
 * where V is a sampled value and W is its weight. The minimum and maximum
 * values of the domains must be provided.
 */
histogram_r(L0,NBins,Min,Max) :-
    maplist(to_pair,L0,L1),
    keysort(L1,L),
    maX <- Max,
    miN <- Min,
    nbinS <- NBins,
    BinWidth <- (maX - miN) / nbinS,
    binwidtH <- BinWidth,
    maplist(key,L,X),
/*    maplist(y,L,Y), */
    x <- c(X),
    <- ggplot() + aes(x) + geom_histogram(binwidth=binwidtH,bins=nbinS).

