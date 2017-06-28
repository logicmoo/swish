% LogicMOO Skel

:- use_module(library(logicmoo_user)).		


%= save compiled clauses using forward chaining storage (by default)
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
:- set_clause_compile(fwc).

%=  setup pfc
:- file_begin(pfc).

%=  Trace execution
:- mpred_trace_exec.

% Your program goes here

a ==> b.

a.

/** <examples> Your example queries go here, e.g.
?- b.
*/
