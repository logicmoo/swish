
:- use_module(library(prolog_autoload)).
:- system:use_module(library(lists),[append/3]).

:- if( \+ exists_source(library(sldnfdraw))).
:- attach_packs('/opt/logicmoo_workspace/packs_lib').
:- endif.
:- if( \+ exists_source(library(lps_syntax))).
:- attach_packs('/opt/logicmoo_workspace/packs_web').
:- endif.


% Debugging
:- debug.
:- tdebug.
:- guitracer.

/*
:- debug(dot).
:- debug(html(script)).
:- debug(hub(_)).
% :- debug(modules).
:- debug(pack(mirror)).
:- debug(pengine(abort)).
:- debug(pldoc).
:- debug(plweb).
:- debug(predicate_options).
:- debug(stats).
:- debug(storage).
:- debug(swish(search)).
:- debug(websocket(_)).
*/

:- debug(http(authenticate)).
:- debug(http(error)).
:- nodebug(http(redirect)).
:- debug(http(request)).
:- debug(http_authenticate).
:- nodebug(http_path).
:- debug(http_session).
:- debug(setting).
:- debug(settings).
:- debug(login).
:- debug(notify(_)).
:- debug(notify).
:- debug(openid(_)).
:- debug(openid).
:- debug(openid_fake(_)).
:- debug(cm(tokens)).
:- debug(authenticate).
:- debug(chat(_)).
:- debug(cm(change)).

:- prolog_ide(debug_monitor).

:- thread_local(prolog_stack:option/2).
:- multifile(prolog_stack:option/2).
:- use_module(library(prolog_stack)).

:- multifile(pengines:allowed/2).
:- dynamic(pengines:allowed/2).
:- multifile(sandbox:safe_primitive/1).
:- dynamic(sandbox:safe_primitive/1).
:- multifile(sandbox:safe_meta_predicate/1).
:- dynamic(sandbox:safe_meta_predicate/1).
:- use_module(library(pengines_sandbox)).
% sandbox:safe_primitive(dumpst:dumpST/0).
sandbox:safe_meta_predicate(system:notrace/1).

:- multifile(cp_menu:menu_item/2).
:- dynamic(cp_menu:menu_item/2).

:- [swish].

:- use_module(library(logicmoo_common)).


:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).

%- thread_httpd:http_server(http_dispatch:http_dispatch,[port('0.0.0.0':3020)]).

:- broadcast(http(post_server_start)).

%pengines_iri_hook(X,Y,Z):- res_iri_hook(X,Y,Z).
%:- register_iri_scheme(pengines, pengines_iri_hook, []).

swish_and_clio:is_module.
%nd_of_file.

add_relative_search_path(Alias, Abs) :-
	is_absolute_file_name(Abs), !,
	prolog_load_context(file, Here),
	relative_file_name(Abs, Here, Rel),
	assertz(user:file_search_path(Alias, Rel)).
add_relative_search_path(Alias, Rel) :-
	assertz(user:file_search_path(Alias, Rel)).

user:file_search_path(cliopatria, '/opt/logicmoo_workspace/packs_web/ClioPatria').
user:file_search_path(swish, '/opt/logicmoo_workspace/packs_web/swish').
user:file_search_path(lps_corner, '/opt/logicmoo_workspace/packs_web/lps_corner').

:- add_relative_search_path(cliopatria, '/opt/logicmoo_workspace/packs_web/ClioPatria').

% Use the ClioPatria help system.  May   be  commented to disable online
% help on the source-code.
:- use_module(cliopatria('applications/help/load')).

% Load ClioPatria itself.  Better keep this line.
:- use_module(cliopatria(cliopatria)).

:- if( \+ current_module(lps_server_UI) ).
:- ensure_loaded(lps_corner(swish/user_module_clio)).
:- endif.


% :- cd('/opt/logicmoo_workspace/packs_web/ClioPatria').
:- cp_server:cp_server.

:- listing(swish_config:authenticate/2).
:- asserta(pengines:allowed(_Request, _Application)).
:- listing(pengines:allowed/2).

:- asserta(cp_menu:menu_item(90=swish/swish, 'Swish Home')).
:- asserta(cp_menu:menu_item(901=swish/swish+class(login), 'Example KB')).
:- asserta(cp_menu:menu_item(300=query/swish, 'SWISH Prolog shell')).

rt123:- rtrace(swish_highlight:codemirror_tokens([protocol(http),method(post),request_uri('/swish/cm/tokens'),
    path('/swish/cm/tokens'),http_version(1-1),host('gitlab.logicmoo.org'),port(3020),
    connection('keep-alive'),cache_control('max-age=0'),peer(ip(192,168,88,1)),
    accept_encoding('gzip, deflate'),accept_language('en-US,en;q=0.9'),cookie(['_ga'='GA1.2.2901774.1587353525',
    swipl_session='2c10-e3c5-65e9-df9a.gitlab']),content_type(html)])).

