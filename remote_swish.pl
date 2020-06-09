/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2016, VU University Amsterdam
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
/*
:- module(swish_ide,
	  [ remote_swish/0,
	    remote_swish/1			% ?Port
	  ]).
*/
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(www_browser)).
:- if(exists_source(library(uid))).
:- use_module(library(uid)).
:- endif.

/* not a * <module>

Open SWISH as an IDE for developing a local application.
*/


from_http(G):- with_output_to(main_error,G).

:- meta_predicate(from_http(0)).


:- use_module(library(must_trace)).


		 /*******************************
		 *	       CONFIG		*
		 *******************************/

:- multifile
	swish_config:config/2,			% Name, Value
	swish_config:source_alias/2,		% Alias, Options
	swish_config:verify_write_access/3,	% Request, File, Options
	pengines:authentication_hook/3,		% Request, Application, User
	pengines:not_sandboxed/2,		% User, Application
	user:file_search_path/2.		% Alias, Path

:- dynamic
	swish_config:config/2,			% Name, Value
	swish_config:source_alias/2,		% Alias, Options
	swish_config:verify_write_access/3,	% Request, File, Options
	pengines:authentication_hook/3,		% Request, Application, User
	pengines:not_sandboxed/2,		% User, Application
	user:file_search_path/2.		% Alias, Path

:- prolog_load_context(directory,Dir),asserta(user:file_search_path(swish, Dir)).
user:file_search_path(project, '.').

:- dynamic http:location/3.
:- multifile http:location/3.
http:location(root, '/', [priority(1100)]).
http:location(swish, root('swish'), [priority(500)]).
% http:location(root, '/remote', []).

rsmsg(_):- !.
rsmsg(X):- wdmsg(X).

/*

%prolog:prolog_load_file(library(swish/X),How):- trace, prolog:load_files([swish(lib/X)],How),!.
%prolog:prolog_load_file(swish(lib/swish/X),How):- prolog:load_files([swish(lib/X)],How),!.
*/

swish_config:config(show_beware,        false).
swish_config:config(community_examples, true).

swish_config:source_alias(project, [access(both), search('*.pl')]).
swish_config:source_alias(library, []).

swish_config:verify_write_access(Request, File, Options) :- currently_logged_in(swish_config:verify_write_access(Request, File, Options),_).

pengines:authentication_hook(Request, swish, User) :- 
   fail, currently_logged_in(pengines:authentication_hook(Request, swish, User),User),!.

:- multifile pengines:allowed/2.
:- dynamic pengines:allowed/2.

pengines:allowed(Request, Application) :- Application=swish-> true; currently_logged_in(pengines:allowed(Request, Application),_User).


pengines:not_sandboxed(Maybe, Application) :- currently_logged_in(pengines:not_sandboxed(Maybe, Application),_User),!.


currently_logged_in(_Why,D):- thread_self(main), ignore(D=default).
currently_logged_in(Why,User):- 
  from_http((http_session:
    (http_in_session(_SessionID),
     http_session_data(oauth2(OAuth, _)),
     http_session_data(user_info(OAuth, Info))),
   User=Info.name,!,
  rsmsg(currently_logged_in(Why,User=Info.name)))),!.
currently_logged_in(Why,User):- 
  from_http((http_session:
    (session_data(S,oauth2(OAuth, Y)),
     rsmsg(currently_logged_in(User=Why,session_data(S,oauth2(OAuth, Y))))))),!,ignore(User="guest1"),!.


currently_logged_in(Why,D):- http_session:http_in_session(SessionID),fail,
  
  from_http(
  ((rsmsg(fail_dispite_http_in_session(SessionID,D,Why)),  
    http_session:http_in_session(SessionID),
    listing(http_session:session_data(SessionID,_Data))))),!,fail.



currently_logged_in(Why,D):- thread_self(S),rsmsg(fail_currently_logged_in(Why,S,D)),!,fail.


no_auth_needed(Request):- is_list(Request),memberchk(path_info(Path),Request),mimetype:file_mime_type(Path,Type),memberchk(Type,[image/_,_/javascript]),!.
no_auth_needed(Request):- is_list(Request),!,memberchk(path(Path),Request),no_auth_needed(Path).
no_auth_needed(X):- \+ atom(X),!,fail.
no_auth_needed(X):- atom_concat('/swish',XX,X),!,no_auth_needed(XX).
no_auth_needed('/chat').
no_auth_needed('/login').
no_auth_needed('/').
no_auth_needed('').


:- multifile swish_config:authenticate/2.
:- dynamic swish_config:authenticate/2.

swish_config:authenticate(_Request, User) :- 
  http_session:
    (http_in_session(_SessionID),
     http_session_data(oauth2(OAuth, _)),
     http_session_data(user_info(OAuth, Info))),
   User=Info.name,!.

swish_config:authenticate(Request, User) :- 
  no_auth_needed(Request),!,
  ignore(currently_logged_in(no_auth_needed,User)),!,
  ignore(User="guest1"),!.

%swish_config:authenticate(Request, User) :- swish_http_authenticate:logged_in(Request, User), !.

swish_config:authenticate(Request, User) :-
 http_session:http_in_session(SessionID),

  currently_logged_in(_Why,User),
  from_http(
  ((http_session:http_in_session(SessionID),
    listing(http_session: session_data(SessionID,_Data))))),
  from_http(rsmsg((http_session:authenticate(Request, User)))),
  !.

swish_config:authenticate(Request, User) :- \+ http_session:http_in_session(_),
  currently_logged_in(_Why,User),
  from_http(rsmsg((swish_config:authenticate(Request, User)))), ignore(User="guest"),!.



  
% swish_config:authenticate(Request, "bad_user") :- rsmsg(swish_config:authenticate(Request, "bad_user")),!.
swish_config:authenticate(Request, User) :- 
        swish_http_authenticate:logged_in(Request, User), !.

  

%%	swish
%
%	Start the SWISH server and open the main page in your browser.

remote_swish :-
	remote_swish('logicmoo.org':3022).

remote_swish(Port) :-
	http_server_property(Port, goal(swish_ide:http_dispatch)), !,
	open_browser(Port).
remote_swish(_:Port) :-
	integer(Port),
	http_server_property(Port, goal(swish_ide:http_dispatch)), !,
	open_browser(Port).
remote_swish(Port) :-
	http_server(http_dispatch,
		    [ port(Port),
		      workers(16)
		    ]),
	open_browser(Port).

open_browser(Address) :-
	host_port(Address, Host, Port),
	http_server_property(Port, scheme(Scheme)),
	http_absolute_location(root(.), Path, []),
	format(atom(URL), '~w://~w:~w~w', [Scheme, Host, Port, Path]),
	rsmsg(www_open_url(URL)).

host_port(Host:Port, Host, Port) :- !.
host_port(Port,Host, Port):- gethostname(Host),!.
host_port(Port,_, Port):-!.


:- [library(pengines)].


pet:- pengine_rpc("http://logicmoo.org:3022",
                       sin_table(X,Y),
                       [ src_text(':- dynamic(sin_table/2). sin_table(1,2).'),
                         application(swish)
                       ]),
   rsmsg(sin_table(X,Y)).

:- ['run_swish_and_clio.pl'].
:- use_module(swish(swish)).


:- listing(swish_config:authenticate/2).

:- stream_property(X,file_no(2)),stream_property(X,alias(main_error)).




:- if(\+ prolog_load_context(reload,true)).
:- remote_swish.
:- endif.


