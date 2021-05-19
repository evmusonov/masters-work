:- module(server,
      [ server/1 ]
).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_server_files)).
:- use_module(library(pengines)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(crypt)).
:- use_module(library(sha)).
:- use_module(library(http/json)).
:- use_module(library(term_to_json)).
:- use_module(library(http/http_header)).
:- use_module(functions).
:- use_module(db_users).
:- use_module(user_module).
:- use_module(admin).
:- use_module(graph).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).
stop_server(Port) :-
	http_stop_server(Port, []).

:- multifile
    user:file_search_path/2,
    http:location/3.
:- dynamic
    user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(app, Dir)).

http:location(js, root(js), []).
http:location(css, root(css), []).
http:location(apps, root(apps), []).
http:location(sapi, root(sapi), []).
user:file_search_path(js_path, './apps/js').
user:file_search_path(css_path, './css').
user:file_search_path(apps, app(apps)).

:- http_handler(apps(.), serve_files_in_directory(apps), [prefix]).
:- http_handler(root(.), index_controller, []).
:- http_handler(root('test-api'), test_api, []).
:- http_handler(js(Filename), http_reply_file(js_path(Filename), []), []).
:- http_handler(css(Filename), http_reply_file(css_path(Filename), []), []).

:- http_handler(sapi(exec), exec_controller, []).
:- http_handler(sapi(execsub), exec_sub_controller, []).
:- http_handler(sapi(execcourse), exec_course_controller, []).

:- initialization db_users:attach_db('db/users').

exec_controller(Request) :-
	http_read_json_dict(Request, JSONDict), !,
	graph:parser(JSONDict.get(str)),graph:graf(_,_,_,G),
	term_to_json(G, X),
	reply_json(X).

exec_course_controller(Request) :-
	http_read_json_dict(Request, JSONDict), !,
	graph:parser(JSONDict.get(str)),
	atom_string(Sub, JSONDict.get(sub)),
	graph:graf(Sub,_,_,G),
	term_to_json(G, X),
	reply_json(X).

exec_sub_controller(Request) :-
	http_read_json_dict(Request, JSONDict), !,
	graph:parser_subs(JSONDict.get(str), S),
	reply_json(S).

index_controller(Request) :-
	user_module:check_session(Request),
	( http_session_data(Message, enter_message)
	-> http_session_retract(Message, enter_message),
	   MessageClass = 'alert alert-warning'
	;  Message = '', MessageClass = ''
	),
    reply_html_page(
	   [
	   	title('Howdy'),
	   	\['<link rel="stylesheet" href="/apps/src/vendor/bootstrap/css/bootstrap.min.css">'],
	   	\html_requires(css('styles.css')),
	   	\['<script src="/apps/js/require.min.js"></script>',
	   	'<script src="/apps/js/lib/scripts.js"></script>']
		%\html_requires([js('require.min.js'), js('lib/scripts.js')])
	   ],
	   [
		div([class='guest-form'], [
			div([class=MessageClass, role='alert'], Message),
			div(class='guest-title', [
				span([class='reg-title'], 'Регистрация'),
				span([class='auth-title'], 'Авторизация')
			]),
			form([class='register-form', method=post, action=location_by_id(register_handler)], [
				label(for=email_input, [
					\['E-mail'],
					input([id=email_input, type=text, name=email])
				]),
				label(for=pass_input, [
					\['Пароль'],
					input([id=pass_input, type=password, name=password])
				]),
				input([type=submit, value='Отправить'])
			]),
			form([class='auth-form', method=post, action=location_by_id(login_handler)], [
				label(for=email_input, [
					\['E-mail'],
					input([id=email_input, type=text, name=email])
				]),
				label(for=pass_input, [
					\['Пароль'],
					input([id=pass_input, type=password, name=password])
				]),
				input([type=submit, value='Войти'])
			])
		])
	   ]).  


test_api(_Request) :-
	format('Access-Control-Allow-Origin: *~nAccess-Control-Allow-Headers: *~n', []),
	reply_json_dict(_{status:'true'}).
  
  
  
  	
