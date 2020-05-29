
/*
:- if(\+ prolog_load_context(reload,true)).
:- abolish(prolog_stack:option/2).
:- multifile(prolog_stack:option/2).
:- thread_local(prolog_stack:option/2).
:- asserta(prolog_stack:option(1,2)).
:- retract(prolog_stack:option(1,2)).
:- prolog_stack:use_module(library(prolog_stack)).
:- endif.
*/

:- system:use_module(library(console_input)).
:- use_module(library(prolog_autoload)).
:- system:use_module(library(lists),[append/3]).

:- if( \+ exists_source(library(sldnfdraw))).
:- attach_packs('/opt/logicmoo_workspace/packs_lib').
:- endif.

:- if( \+ exists_source(library(lps_syntax)); \+ exists_source(pack(plweb/pack_info))).
:- attach_packs('/opt/logicmoo_workspace/packs_web').
:- endif.


:- if( current_prolog_flag(xpce,true) ).

% Debugging
:- prolog_ide(debug_monitor).
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


:- endif.

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

:- user:[swish].

% :- use_module(library(logicmoo_common)).


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
:- user:use_module(cliopatria('applications/help/load')).

% Load ClioPatria itself.  Better keep this line.
:- user:use_module(cliopatria(cliopatria)).

:- if( \+ current_module(lps_server_UI) ).
% :- ensure_loaded(lps_corner(swish/user_module_clio)).
:- endif.


% :- cd('/opt/logicmoo_workspace/packs_web/ClioPatria').
:- if(\+ prolog_load_context(reload,true)).
:- listing(cp_server:cp_server).
:- cp_server:cp_server.
:- endif.

:- listing(swish_config:authenticate/2).
:- asserta(pengines:allowed(_Request, _Application)).
:- listing(pengines:allowed/2).

:- asserta(cp_menu:menu_item(90=swish/swish, 'Swish Home')).
:- asserta(cp_menu:menu_item(300=query/swish, 'SWISH Prolog shell')).

rt123:- rtrace(swish_highlight:codemirror_tokens([protocol(http),method(post),request_uri('/swish/cm/tokens'),
    path('/swish/cm/tokens'),http_version(1-1),host('gitlab.logicmoo.org'),port(3020),
    connection('keep-alive'),cache_control('max-age=0'),peer(ip(192,168,88,1)),
    accept_encoding('gzip, deflate'),accept_language('en-US,en;q=0.9'),cookie(['_ga'='GA1.2.2901774.1587353525',
    swipl_session='2c10-e3c5-65e9-df9a.gitlab']),content_type(html)])).

:- user:ensure_loaded(library(lps_corner)).



% X = lps_visualization(_1422{groups:[_2144{content:"left(A)", id:"left/1", order:3, subgroupStack:"false"}, _2190{content:"right(A)", id:"right/1", order:3, subgroupStack:"false"}, _2238{content:"searching(A)", id:"searching/1", order:3, subgroupStack:"false"}, _2286{content:"Actions", id:"action", order:4}], items:[_1440{content:"0", end:2, group:"left/1", id:0, start:1, subgroup:"0", title:"Fluent left(0) initiated at 1<br/>and terminated at transition to 2"}, _1518{content:"5", end:4, group:"left/1", id:1, start:2, subgroup:"5", title:"Fluent left(5) initiated at 2<br/>and terminated at transition to 4"}, _1596{content:"7", end:21, group:"left/1", id:2, start:4, subgroup:"7", title:"Fluent left(7) initiated at 4<br/>and terminated at transition to 21"}, _1674{content:"7", end:21, group:"right/1", id:3, start:3, subgroup:"7", title:"Fluent right(7) initiated at 3<br/>and terminated at transition to 21"}, _1754{content:"9", end:3, group:"right/1", id:4, start:1, subgroup:"9", title:"Fluent right(9) initiated at 1<br/>and terminated at transition to 3"}, _872{content:"60", end:21, group:"searching/1", id:5, start:1, subgroup:"60", title:"Fluent searching(60) initiated at 1<br/>and terminated at transition to 21"}, _954{content:"sample(4)", group:"action", id:6, start:2, style:"color:green", title:"happens(sample(4),1,2)", type:"point"}, _1030{content:"sample(7)", group:"action", id:7, start:3, style:"color:green", title:"happens(sample(7),2,3)", type:"point"}, _1106{content:"sample(6)", group:"action", id:8, start:4, style:"color:green", title:"happens(sample(6),3,4)", type:"point"}]}, [])+dot(digraph([node([1], [label = 'left(0)\nright(9)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color=black]), node([3], [label = 'left(5)\nright(7)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), node([2], [label = 'left(5)\nright(9)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), node([4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20], [label = 'left(7)\nright(7)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), edge(([1]->[2]), [label=sample(4), color=forestgreen]), edge(([2]->[3]), [label=sample(7), color=forestgreen]), edge(([3]->[4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]), [label=sample(6), color=forestgreen])])).
% X = dot(digraph([node([1], [label = 'left(0)\nright(9)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color=black]), node([3], [label = 'left(5)\nright(7)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), node([2], [label = 'left(5)\nright(9)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), node([4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20], [label = 'left(7)\nright(7)\nsearching(60)', fillcolor = '#D7DCF5', style=filled, color = '#D7DCF5']), edge(([1]->[2]), [label=sample(4), color=forestgreen]), edge(([2]->[3]), [label=sample(7), color=forestgreen]), edge(([3]->[4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]), [label=sample(6), color=forestgreen])])).

