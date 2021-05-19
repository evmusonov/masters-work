:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(crypt)).
:- use_module(library(debug)).
:- use_module(library(http/http_path)).


server(Port) :-
        http_server(http_dispatch, [port(Port)]).

:- http_handler(root(.), index_controller, []).
:- http_handler(root(login), login_controller, []).
:- http_handler(root(user), user_controller, []).
:- http_handler(root(user/session), see_session, []).
:- http_handler(root(user/u1), user1_controller, []).
:- http_handler(root('add-session'), session_controller, []).
:- http_handler(root('del-session'), del_session_controller, []).


:- if(exists_source(library(http/http_digest))).
:- use_module(library(http/http_digest)).
:- setting(method, oneof([basic,digest]), digest,
	   "HTTP authentication method used").
:- else.
:- setting(method, oneof([basic]), basic,
	   "HTTP authentication method used").
:- endif.

:- setting(realm, atom, 'SWISH user',
	   "HTTP authentication realm").
:- setting(password_file, callable, passwd,
	   "Location of the password file").

:- dynamic
	password_file_cache/1.

password_file(File) :-
	password_file_cache(File), !.
password_file(File) :-
	setting(password_file, Spec),
	absolute_file_name(Spec, File, [access(read)]),
	update_auth_type(File),
	asserta(password_file_cache(File)).

update_auth_type(File) :-
	digest_password_file(File), !,
	setting(method, Method),
	(   Method == digest
	->  true
	;   print_message(warning, http_auth_type(Method, digest)),
	    set_setting(method, digest)
	).
update_auth_type(_) :-
	setting(method, Method),
	(   Method == basic
	->  true
	;   print_message(warning, http_auth_type(Method, basic)),
	    set_setting(method, basic)
	).

digest_password_file(File) :-
	http_read_passwd_file(File, [passwd(_User, Hash, _Fields)|_]),
	is_sha1(Hash).

is_sha1(Hash) :-
	atom_length(Hash, 32),
	forall(sub_atom(Hash, _, 1, _, Char),
	       char_type(Char, xdigit(_))).
	       
	       

user_controller(_Request) :-
	http_session_asserta(hello, session1),
	format('Content-type: text/html~n~n', []),
	format('okay', []).
	
user1_controller(_Request) :-
	http_session_asserta(hello1, session1),
	format('Content-type: text/html~n~n', []),
	format('okay', []).

see_session(_Request) :-
	( http_session_data(Data, session1)
	-> format('Content-type: text/html~n~n', []),
	   format('~a', [Data])
	;  format('Content-type: text/html~n~n', []),
	   format('No data', [])
	).

del_session_controller(_Request) :-
	http_session_retract(hello),
	format('Content-type: text/html~n~n', []),
	format('deleted', []).
	
	
login_controller(Request) :-
	login(Request, john).


login(Request, User) :-
        logged_in(Request, User).

logged_in(Request, User) :-
	password_file(File),
	( http_authenticate(basic(File), Request, [User|_Fields])
	-> format('Content-type: text/html~n~n', []),
	   format('Hello, ~a!', [User])
	; throw(http_reply(authorise(basic(passwd))))
	).



index_controller(_Request) :-
	add_user(john1, '12345', []).
	
add_user(User, Passwd, Fields) :-
	phrase("$1$", E, _),		% use Unix MD5 hashes
	crypt(Passwd, E),
	string_codes(Hash, E),
	update_password(passwd(User, Hash, Fields)).	    

update_password(Entry) :-
	arg(1, Entry, User),
	writeable_passwd_file(File),
	(   exists_file(File)
	->  http_read_passwd_file(File, Data)
	;   Data = []
	),
	(   selectchk(passwd(User, _, _), Data, Entry, NewData)
	->  true
	;   append(Data, [Entry], NewData)
	),
	http_write_passwd_file(File, NewData).

writeable_passwd_file(File) :-
	(   catch(password_file(File), _, fail)
	->  true
	;   setting(password_file, Spec),
	    absolute_file_name(Spec, File, [access(write)])
	).
	